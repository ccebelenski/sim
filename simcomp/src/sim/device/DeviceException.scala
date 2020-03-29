package sim.device

import sim.SimException

/**
  * Created by christophercebelenski on 7/19/16.
  */
class DeviceException(device: BasicDevice, message: String, cause: Throwable) extends SimException(message: String, cause: Throwable) {

}
