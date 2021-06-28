{-# LANGUAGE DataKinds #-}

-- | Vulkan pipelines
module Popcorn.Engine.Renderer.Vulkan.Pipeline
    ( -- * Data types
      PipelineDef(..)

      -- * Resource creation/release
    , withPipelines
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Managed (MonadManaged)
import Data.Bits ((.|.))
import Data.Foldable (traverse_)
import Data.Word (Word32)

import Popcorn.Engine.Exception (EngineException(EngineException))
import Popcorn.Engine.Managed.Extra (bracketManaged)

import qualified Data.Vector as V
import qualified Vulkan.Core10 as Vk
import qualified Vulkan.CStruct.Extends as Vk
import qualified Vulkan.Zero as Vk
import qualified Data.Text as T

-- | Pipeline definition
data PipelineDef = PipelineDef
    { pdRenderPass :: Vk.RenderPass
    , pdSubpassIndex :: Word32
    , pdVS :: Vk.ShaderModule
    , pdFS :: Vk.ShaderModule
    , pdOffset :: Vk.Offset2D
    , pdExtent :: Vk.Extent2D
    }

-- | Bracket for creating/releasing Vulkan pipelines
withPipelines
    :: MonadManaged m
    => Vk.Device
    -> V.Vector PipelineDef
    -> m (V.Vector Vk.Pipeline)
withPipelines device defs = do
    pipelineLayout <- Vk.withPipelineLayout device
        mkPipelineLayoutCreateInfo Nothing bracketManaged 

    bracketManaged (createPipelines device pipelineLayout defs)
        (destroyPipelines device)

mkPipelineLayoutCreateInfo :: Vk.PipelineLayoutCreateInfo
mkPipelineLayoutCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.setLayouts = []
    , Vk.pushConstantRanges = []
    }

-- Creates graphics pipelines
createPipelines
    :: Vk.Device
    -> Vk.PipelineLayout
    -> V.Vector PipelineDef
    -> IO (V.Vector Vk.Pipeline)
createPipelines device pipelineLayout defs = do
    (result, pipelines) <- Vk.createGraphicsPipelines device
        Vk.NULL_HANDLE (mkGraphicsPipelineCreateInfo pipelineLayout defs) Nothing

    when (result /= Vk.SUCCESS) $ throwIO (EngineException
        ("Error, could not create graphics pipeline! = " <> T.pack (show result)))
    
    return pipelines

-- Destroys existing graphics Vulkan pipelines
destroyPipelines :: Vk.Device -> V.Vector Vk.Pipeline -> IO ()
destroyPipelines = traverse_ . destroyPipeline

destroyPipeline :: Vk.Device -> Vk.Pipeline -> IO ()
destroyPipeline device pipeline = Vk.destroyPipeline device pipeline Nothing

mkGraphicsPipelineCreateInfo
    :: Vk.PipelineLayout
    -> V.Vector PipelineDef
    -> V.Vector (Vk.SomeStruct Vk.GraphicsPipelineCreateInfo)
mkGraphicsPipelineCreateInfo pipelineLayout = fmap
    (Vk.SomeStruct . createPipelineInfoFromDef pipelineLayout)

createPipelineInfoFromDef
    :: Vk.PipelineLayout
    -> PipelineDef
    -> Vk.GraphicsPipelineCreateInfo '[]
createPipelineInfoFromDef pipelineLayout PipelineDef{..} = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.stages =
        [ Vk.SomeStruct (mkPipelineShaderStageCreateInfoVS pdVS)
        , Vk.SomeStruct (mkPipelineShaderStageCreateInfoFS pdFS)
        ]
    , Vk.vertexInputState = Just (Vk.SomeStruct mkPipelineVertexInputStateCreateInfo)
    , Vk.inputAssemblyState = Just mkPipelineInputAssemblyStateCreateInfo
    , Vk.tessellationState = Nothing
    , Vk.viewportState = Just (Vk.SomeStruct
        (mkPipelineViewportStateCreateInfo pdOffset pdExtent))
    , Vk.rasterizationState = Vk.SomeStruct mkPipelineRasterizationStateCreateInfo
    , Vk.multisampleState = Just (Vk.SomeStruct mkPipelineMultisampleStateCreateInfo)
    , Vk.depthStencilState = Nothing
    , Vk.colorBlendState = Just (Vk.SomeStruct mkPipelineColorBlendStateCreateInfo)
    , Vk.dynamicState = Nothing
    , Vk.layout = pipelineLayout
    , Vk.renderPass = pdRenderPass
    , Vk.subpass = pdSubpassIndex
    , Vk.basePipelineHandle = Vk.NULL_HANDLE
    , Vk.basePipelineIndex = -1
    }

mkPipelineShaderStageCreateInfoVS
    :: Vk.ShaderModule
    -> Vk.PipelineShaderStageCreateInfo '[]
mkPipelineShaderStageCreateInfoVS shaderModule = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.stage = Vk.SHADER_STAGE_VERTEX_BIT
    , Vk.module' = shaderModule
    , Vk.name = "main"
    , Vk.specializationInfo = Nothing
    }

mkPipelineShaderStageCreateInfoFS
    :: Vk.ShaderModule
    -> Vk.PipelineShaderStageCreateInfo '[]
mkPipelineShaderStageCreateInfoFS shaderModule = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.stage = Vk.SHADER_STAGE_FRAGMENT_BIT
    , Vk.module' = shaderModule
    , Vk.name = "main"
    , Vk.specializationInfo = Nothing
    }

mkPipelineVertexInputStateCreateInfo :: Vk.PipelineVertexInputStateCreateInfo '[]
mkPipelineVertexInputStateCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.vertexBindingDescriptions = []
    , Vk.vertexAttributeDescriptions = []
    }

mkPipelineInputAssemblyStateCreateInfo :: Vk.PipelineInputAssemblyStateCreateInfo
mkPipelineInputAssemblyStateCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.topology = Vk.PRIMITIVE_TOPOLOGY_TRIANGLE_LIST
    , Vk.primitiveRestartEnable = False   
    }

mkPipelineViewportStateCreateInfo
    :: Vk.Offset2D
    -> Vk.Extent2D
    -> Vk.PipelineViewportStateCreateInfo '[]
mkPipelineViewportStateCreateInfo offset extent = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.viewports = [Vk.Viewport (fromIntegral x) (fromIntegral y)
        (fromIntegral w) (fromIntegral h) 0.0 1.0]
    , Vk.scissors = [Vk.Rect2D offset extent]
    }
  where
    Vk.Offset2D x y = offset
    Vk.Extent2D w h = extent

mkPipelineRasterizationStateCreateInfo :: Vk.PipelineRasterizationStateCreateInfo '[]  
mkPipelineRasterizationStateCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.depthClampEnable = False
    , Vk.rasterizerDiscardEnable = False
    , Vk.polygonMode = Vk.POLYGON_MODE_FILL
    , Vk.cullMode = Vk.CULL_MODE_BACK_BIT
    , Vk.frontFace = Vk.FRONT_FACE_COUNTER_CLOCKWISE
    , Vk.depthBiasEnable = False
    , Vk.depthBiasConstantFactor = 0.0
    , Vk.depthBiasClamp = 0.0
    , Vk.depthBiasSlopeFactor = 0.0
    , Vk.lineWidth = 1.0
    }

mkPipelineMultisampleStateCreateInfo :: Vk.PipelineMultisampleStateCreateInfo '[] 
mkPipelineMultisampleStateCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.rasterizationSamples = Vk.SAMPLE_COUNT_1_BIT
    , Vk.sampleShadingEnable = False
    , Vk.minSampleShading = 0.0
    , Vk.sampleMask = [] 
    , Vk.alphaToCoverageEnable = False
    , Vk.alphaToOneEnable = False
    }

mkPipelineColorBlendStateCreateInfo :: Vk.PipelineColorBlendStateCreateInfo '[]
mkPipelineColorBlendStateCreateInfo = Vk.zero
    { Vk.flags = Vk.zero
    , Vk.logicOpEnable = False
    , Vk.logicOp = Vk.LOGIC_OP_COPY 
    , Vk.attachments = [mkPipelineColorBlendAttachmentState]
    , Vk.blendConstants = (0, 0, 0, 0)
    }

mkPipelineColorBlendAttachmentState :: Vk.PipelineColorBlendAttachmentState
mkPipelineColorBlendAttachmentState = Vk.zero
    { Vk.blendEnable = False
    , Vk.srcColorBlendFactor = Vk.BLEND_FACTOR_ONE
    , Vk.dstColorBlendFactor = Vk.BLEND_FACTOR_ZERO
    , Vk.colorBlendOp = Vk.BLEND_OP_ADD
    , Vk.srcAlphaBlendFactor = Vk.BLEND_FACTOR_ONE
    , Vk.dstAlphaBlendFactor = Vk.BLEND_FACTOR_ZERO
    , Vk.alphaBlendOp = Vk.BLEND_OP_ADD
    , Vk.colorWriteMask = Vk.COLOR_COMPONENT_R_BIT
        .|. Vk.COLOR_COMPONENT_G_BIT
        .|. Vk.COLOR_COMPONENT_B_BIT
        .|. Vk.COLOR_COMPONENT_A_BIT
    }