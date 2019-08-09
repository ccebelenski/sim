package com.sim.cpu

class PDP11MMU(cpu: PDP11) extends BasicMMU(cpu) {


  // Registers
  val MMR0 = new Register16("MMR0") // MMR0 - Status
  val MMR1 = new Register16("MMR1") // MMR1 - R+/-R
  val MMR2 = new Register16("MMR2") // MMR2 - saved PC
  val MMR3 = new Register16("MMR3") // MMR3 - 22b status

  val KIPAR0 = new Register16("KIPARO")
  val KIPDR0 = new Register16("KIPDRO")
  val KIP0 = new CompositeRegister32("KIP0",KIPAR0,KIPDR0)



  MMR0(0)
  MMR1(0)
  MMR2(0)
  MMR3(0)
  KIP0(0)


  val APRFILE : Array[CompositeRegister32] = Array(KIP0)


}
