package parser

sealed trait MemorySegment

case object LCL extends MemorySegment
case object ARG extends MemorySegment
case object THIS extends MemorySegment
case object THAT extends MemorySegment
case object CONSTANT extends MemorySegment
case object TEMP extends MemorySegment
case object STATIC extends MemorySegment
case object POINTER extends MemorySegment
