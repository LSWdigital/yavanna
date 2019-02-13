; SPIR-V
; Version: 1.0
; Generator: Hand Written


		OpCapability Shader
		OpMemoryModel Logical Simple
		OpEntryPoint Fragment %main "main" %outColor %fragColor
		OpExecutionMode %main OriginUpperLeft

		OpName %main "main"
		OpName %outColor "outColor"
		OpName %fragColor "fragColor"

		OpDecorate %fragColor Location 0	
		OpDecorate %outColor Location 0

%void = OpTypeVoid
%fun  = OpTypeFunction %void
%float = OpTypeFloat 32
%int  = OpTypeInt 32 0
%bool = OpTypeBool
%vec4 = OpTypeVector %float 4
%v3float = OpTypeVector %float 3
%19 = OpTypeFunction %vec4 %vec4 %int

%vfun = OpTypePointer Function %vec4

%v4Out = OpTypePointer Output %vec4
%outColor = OpVariable %v4Out Output

%v3In = OpTypePointer Input %v3float
%fragColor = OpVariable %v3In Input

%1 	  = OpConstant %float 1
%zero = OpConstant %float 0
%int0 = OpConstant %int 0
%int1 = OpConstant %int 1
%p1   = OpConstant %float 0.1
%five = OpConstant %int 5

; Recursive function vec4 -> int -> vec4
%recF = OpFunction %vec4 Pure %19
%rF = OpFunctionParameter %vec4
%rI = OpFunctionParameter %int
%init = OpLabel

; A variable fval
%fval = OpVariable %vfun Function

; Branch and merge at ret
 %cond = OpIEqual %bool %rI %int0
	OpSelectionMerge %ret None
 	OpBranchConditional %cond %fin %rec

; Add 0.1 to each part of the composite
%rec  = OpLabel
%v1   = OpCompositeConstruct %vec4 %p1 %p1 %p1 %zero
%v2   = OpFAdd %vec4 %rF %v1
%i1   = OpISub %int %rI %int1
; Call recursively (tail recursive)
%rval = OpFunctionCall %vec4 %recF %v2 %i1
		OpStore %fval %rval
;		OpStore %fval %v2
	OpBranch %ret
		
; Alternatively, do nothing
%fin  = OpLabel
		OpStore %fval %rF
		OpBranch %ret

; Return fval
%ret  = OpLabel
%retval = OpLoad %vec4 %fval
 		OpReturnValue %retval
	OpFunctionEnd




%main = OpFunction %void None %fun
%lab  = OpLabel

%dark = OpCompositeConstruct %vec4 %zero %zero %zero %1

%lite = OpFunctionCall %vec4 %recF %dark %int0
		
		OpStore %outColor %lite
		OpReturn
		OpFunctionEnd

