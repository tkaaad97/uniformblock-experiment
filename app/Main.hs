{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import Control.Exception (throwIO)
import Control.Monad (unless)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as ByteString (pack, unpack,
                                                      useAsCStringLen)
import Data.Text (Text)
import qualified Data.Text as Text (unpack)
import qualified Data.Text.Encoding as Text (encodeUtf8)
import Foreign (Storable(..))
import qualified Foreign (Ptr, alloca, allocaArray, peek, with)
import qualified Foreign.C.String as Foreign (peekCStringLen)
import qualified Graphics.GL as GL
import Text.Shakespeare.Text (st)
import Window

main :: IO ()
main = withWindow 400 300 "test" go f
    where
    go _ = do
        results <- mapM (investigateUniformBlock "") uniformTypes
        rowmatResults <- mapM (investigateUniformBlock "layout(row_major) uniform;") matrixUniformTypes
        colmatResults <- mapM (investigateUniformBlock "layout(column_major) uniform;") matrixUniformTypes
        putStrLn "|uniform type|alignment|byte size|array stride|matrix stride|matrix order|"
        putStrLn "|:-----------|-----:|--------:|-----------:|------------:|:----------:|"
        mapM printResult (results ++ rowmatResults ++ colmatResults)
    f _ _ = return ()

    uniformTypes =
        [ "bvec2"
        , "bvec3"
        , "bvec4"
        , "vec2"
        , "vec3"
        , "vec4"
        , "ivec2"
        , "ivec3"
        , "ivec4"
        , "dvec2"
        , "dvec3"
        , "dvec4"
        , "float[4]"
        , "vec2[4]"
        , "vec3[4]"
        , "vec4[4]"
        , "dvec2[4]"
        , "dvec3[4]"
        , "dvec4[4]"
        ]

    matrixUniformTypes =
        [ "mat2"
        , "mat2x3"
        , "mat2x4"
        , "mat3"
        , "mat3x2"
        , "mat3x4"
        , "mat4"
        , "mat4x2"
        , "mat4x3"
        , "dmat2"
        , "dmat2x3"
        , "dmat2x4"
        , "dmat3"
        , "dmat3x2"
        , "dmat3x4"
        , "dmat4"
        , "dmat4x2"
        , "dmat4x3"
        , "mat2[4]"
        , "mat2x3[4]"
        , "mat2x4[4]"
        , "mat3[4]"
        , "mat3x2[4]"
        , "mat3x4[4]"
        , "mat4[4]"
        , "mat4x2[4]"
        , "mat4x3[4]"
        , "dmat2[4]"
        , "dmat2x3[4]"
        , "dmat2x4[4]"
        , "dmat3[4]"
        , "dmat3x2[4]"
        , "dmat3x4[4]"
        , "dmat4[4]"
        , "dmat4x2[4]"
        , "dmat4x3[4]"
        ]

data UniformInfo = UniformInfo
    { uiName               :: !ByteString
    , uiOffset             :: !Int
    , uiArrayStride        :: !(Maybe Int)
    , uiMatrixStride       :: !(Maybe Int)
    , uiMatrixStorageOrder :: !(Maybe MatrixStorageOrder)
    } deriving (Show, Eq)

data Result = Result !Text !Int !Int !(Maybe Int) !(Maybe Int) !(Maybe MatrixStorageOrder)
    deriving (Show, Eq)

data MatrixStorageOrder =
    RowMajor |
    ColumnMajor
    deriving (Show, Eq)

investigateUniformBlock :: Text -> Text -> IO Result
investigateUniformBlock option uniformType = do
    let vsource = genVertexShaderSource
        fsource = genFragmentShaderSource option uniformType
    program <- mkProgram vsource fsource
    uniformInfos <- getUniformInfos program
    let u0 = uniformInfos !! 0
        u1 = uniformInfos !! 1
        u2 = uniformInfos !! 2
        result = Result uniformType (uiOffset u1) (uiOffset u2 - uiOffset u1) (uiArrayStride u1) (uiMatrixStride u1) (uiMatrixStorageOrder u1)
    GL.glDeleteProgram program
    return result

getUniformInfos :: GL.GLuint -> IO [UniformInfo]
getUniformInfos program = do
    len  <- Foreign.alloca $ \p -> GL.glGetProgramiv program GL.GL_ACTIVE_UNIFORMS p >> Foreign.peek p
    mapM g [0..(len - 1)]

    where
    maxNameBytes = 128
    g i =
        Foreign.alloca $ \lp ->
        Foreign.alloca $ \sp ->
        Foreign.alloca $ \tp ->
        Foreign.alloca $ \ip ->
        Foreign.alloca $ \op ->
        Foreign.allocaArray maxNameBytes $ \np -> do
            GL.glGetActiveUniform program (fromIntegral i) (fromIntegral maxNameBytes) lp sp tp np
            len <- Foreign.peek lp
            name <- ByteString.pack <$> Foreign.peekCStringLen (np, fromIntegral len)

            Foreign.poke ip (fromIntegral i)
            GL.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_OFFSET op
            offset <- Foreign.peek op

            GL.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_ARRAY_STRIDE op
            astride <- Foreign.peek op

            GL.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_MATRIX_STRIDE op
            mstride <- Foreign.peek op

            GL.glGetActiveUniformsiv program 1 ip GL.GL_UNIFORM_IS_ROW_MAJOR op
            isRowMajor <- Foreign.peek op

            let offset' = fromIntegral offset
                astride' = if astride > 0 then Just . fromIntegral $ astride else Nothing
                mstride' = if mstride > 0 then Just . fromIntegral $ mstride else Nothing
                order = if mstride > 0
                            then if isRowMajor == 1 then Just RowMajor else Just ColumnMajor
                            else Nothing

            return (UniformInfo name offset' astride' mstride' order)

printResult :: Result -> IO ()
printResult (Result name offset size astride mstride order) = do
    let astride' = maybe "" show astride
        mstride' = maybe "" show mstride
        showOrder Nothing            = ""
        showOrder (Just RowMajor)    = "row"
        showOrder (Just ColumnMajor) = "column"
    putStrLn $ "|" ++ Text.unpack name ++ "|" ++ show offset ++ "|" ++ show size ++ "|" ++ astride' ++ "|" ++ mstride' ++ "|" ++ showOrder order ++ "|"

mkProgram :: ByteString -> ByteString -> IO GL.GLuint
mkProgram vsource fsource = do
    vshader <- mkShader GL.GL_VERTEX_SHADER vsource
    fshader <- mkShader GL.GL_FRAGMENT_SHADER fsource
    program <- GL.glCreateProgram
    GL.glAttachShader program vshader
    GL.glAttachShader program fshader
    GL.glLinkProgram program
    throwIfProgramErrorStatus program GL.GL_LINK_STATUS "program link error"
    GL.glDeleteShader vshader
    GL.glDeleteShader fshader
    return program

mkShader :: GL.GLenum -> ByteString -> IO GL.GLuint
mkShader shaderType source =
    ByteString.useAsCStringLen source $ \(source', len) ->
    Foreign.with source' $ \sp ->
    Foreign.with (fromIntegral len) $ \lp -> do
        shader <- GL.glCreateShader shaderType
        GL.glShaderSource shader 1 sp lp
        GL.glCompileShader shader
        throwIfShaderErrorStatus shader GL.GL_COMPILE_STATUS "shader compile error"
        return shader

throwIfProgramErrorStatus
    :: GL.GLuint
    -> GL.GLenum
    -> String
    -> IO ()
throwIfProgramErrorStatus program statusName messagePrefix = Foreign.alloca $ \p -> do
    GL.glGetProgramiv program statusName p
    status <- Foreign.peek p
    unless (status == GL.GL_TRUE) $
        Foreign.alloca $ \sizePtr ->
        Foreign.allocaArray bufSize $ \buf -> do
            GL.glGetProgramInfoLog program (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log' <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log'
    where
    bufSize = 256

throwIfShaderErrorStatus
    :: GL.GLuint
    -> GL.GLenum
    -> String
    -> IO ()
throwIfShaderErrorStatus shader statusName messagePrefix = Foreign.alloca $ \p -> do
    GL.glGetShaderiv shader statusName p
    status <- Foreign.peek p
    unless (status == GL.GL_TRUE) $
        Foreign.alloca $ \sizePtr ->
        Foreign.allocaArray bufSize $ \buf -> do
            GL.glGetShaderInfoLog shader (fromIntegral bufSize) sizePtr buf
            logSize <- Foreign.peek sizePtr
            log' <- Foreign.peekCStringLen (buf, fromIntegral logSize)
            throwIO . userError $ messagePrefix ++ ": " ++ log'
    where
    bufSize = 256

genVertexShaderSource :: ByteString
genVertexShaderSource = Text.encodeUtf8 source
    where
    source = [st|#version 450
void main()
{
    gl_Position = vec4(1.0);
}
|]

genFragmentShaderSource :: Text -> Text -> ByteString
genFragmentShaderSource option uniformType = Text.encodeUtf8 code
    where
    code = [st|#version 450

#{option}

out vec4 outColor;

layout (std140) uniform ublock {
    float uniform1;
    #{uniformType} uniform2;
    float uniform3;
} ublock;

void main()
{
    outColor = vec4(1.0);
}
|]
