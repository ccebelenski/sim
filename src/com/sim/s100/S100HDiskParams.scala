package com.sim.s100

/**
  * Parameters for the different drive types the HDisk driver supports.
  */
abstract class S100HDiskParamsBase {

  // Name of CP/M parameter block
  var name: String

  // Description
  var desc: String

  // capacity
  var capac: Long

  // sectors per track
  var spt: Int

  /* data allocation block shift factor                       */
  var bsh: Int

  /* data allocation block mask                               */
  var blm: Int

  /* extent mask                                              */
  var exm: Int

  /* maximum data block number                                */
  var dsm: Int

  /* total number of directory entries                        */
  var drm: Int

  /* determine reserved directory blocks                      */
  var al0: Int

  /* determine reserved directory blocks                      */
  var al1: Int

  /* size of directory check vector                           */
  var cks: Int

  /* number of reserved tracks                                */
  var off: Int

  /* physical record shift factor, CP/M 3                     */
  var psh: Int

  /* physical record mask, CP/M 3                             */
  var phm: Int

  /* 0 for 128 << psh, > 0 for special                        */
  var physicalSectorSize: Int

  /* offset in physical sector where logical sector starts    */
  var offset: Int

  /* Skew table or None                            */
  var skew: Option[Array[Int]]


  /*
  typedef struct {
    char    name[DPB_NAME_LENGTH + 1];  // name of CP/M disk parameter block                        
    t_addr  capac;                      // capacity                                                 
    uint32  spt;                        // sectors per track                                        
    uint8   bsh;                        // data allocation block shift factor                       
    uint8   blm;                        // data allocation block mask                               
    uint8   exm;                        // extent mask                                              
    uint16  dsm;                        // maximum data block number                                
    uint16  drm;                        // total number of directory entries                        
    uint8   al0;                        // determine reserved directory blocks                      
    uint8   al1;                        // determine reserved directory blocks                      
    uint16  cks;                        // size of directory check vector                           
    uint16  off;                        // number of reserved tracks                                
    uint8   psh;                        // physical record shift factor, CP/M 3                     
    uint8   phm;                        // physical record mask, CP/M 3                             
    int32   physicalSectorSize;         // 0 for 128 << psh, > 0 for special                        
    int32   offset;                     // offset in physical sector where logical sector starts    
    int32   *skew;                      // pointer to skew table or NULL                            
} DPB;
   */

  // Skew tables
  var standard8: Array[Int] = Array(0, 6, 12, 18, 24, 4, 10, 16,
    22, 2, 8, 14, 20, 1, 7, 13,
    19, 25, 5, 11, 17, 23, 3, 9,
    15, 21)

  var apple_ii_DOS: Array[Int] = Array(0, 6, 12, 3, 9, 15, 14, 5,
    11, 2, 8, 7, 13, 4, 10, 1)

  var apple_ii_DOS2: Array[Int] = Array(0, 1, 12, 13, 24, 25, 6, 7,
    18, 19, 30, 31, 28, 29, 10, 11,
    22, 23, 4, 5, 16, 17, 14, 15,
    26, 27, 8, 9, 20, 21, 2, 3)

  var apple_ii_PRODOS: Array[Int] = Array(0, 9, 3, 12, 6, 15, 1, 10,
    4, 13, 7, 8, 2, 11, 5, 14)

  var apple_ii_PRODOS2: Array[Int] = Array(0, 1, 18, 19, 6, 7, 24, 25,
    12, 13, 30, 31, 2, 3, 20, 21,
    8, 9, 26, 27, 14, 15, 16, 17,
    4, 5, 22, 23, 10, 11, 28, 29)

  var mits: Array[Int] = Array(0, 17, 2, 19, 4, 21, 6, 23,
    8, 25, 10, 27, 12, 29, 14, 31,
    16, 1, 18, 3, 20, 5, 22, 7,
    24, 9, 26, 11, 28, 13, 30, 15)

  def verifyDiskInfo():Unit = {
    if()
  }

}

// Note in the following CKS = 0 for fixed media which are not supposed to be
// changed while CP/M is executing. Also note that spt (sectors per track) is
// measured in CP/M sectors of size 128 bytes. Standard format "HDSK" must be
// first as index 0 is used as default in some cases.


class HDSK extends S100HDiskParamsBase {
  override var name: String = "HDSK"
  override var desc: String = "AZ80 HDSK"
  override var capac: Long = S100HDSKDevice.HDSK_CAPACITY
  override var spt: Int = 32
  override var bsh: Int = 0x05
  override var blm: Int = 0x1F
  override var exm: Int = 0x01
  override var dsm: Int = 0x07F9
  override var drm: Int = 0x03FF
  override var al0: Int = 0xFF
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0006
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None
}

class CPM68K extends S100HDiskParamsBase {
  override var name: String = "CPM68K"
  override var desc: String = "CP/M-68K HDSK"
  override var capac: Long =  1 << 24
  override var spt: Int = 1<<17
  override var bsh: Int = 0
  override var blm: Int = 0
  override var exm: Int = 0
  override var dsm: Int = 0
  override var drm: Int = 0
  override var al0: Int = 0
  override var al1: Int = 0
  override var cks: Int = 0
  override var off: Int = 0
  override var psh: Int = 0
  override var phm: Int = 0
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}

class EZ80FL extends S100HDiskParamsBase {
  override var name: String = "EZ80FL"
  override var desc: String = "128K FLASH"
  override var capac: Long = 131072
  override var spt: Int = 32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 127
  override var drm: Int = 0x003E
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0000
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}

class P112 extends S100HDiskParamsBase {
  override var name: String = "P112"
  override var desc: String = "1.44M P112"
  override var capac: Long = 1474560
  override var spt: Int = 72
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 710
  override var drm: Int = 0x00FE
  override var al0: Int = 0xF0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class SU720 extends S100HDiskParamsBase {
  override var name: String = "SU720"
  override var desc: String = "720K Super I/O"
  override var capac: Long = 737280
  override var spt: Int = 36
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 354
  override var drm: Int = 0x007E
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0020
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class OSB1 extends S100HDiskParamsBase {
  override var name: String = "OSB1"
  override var desc: String = "Osborne1 5.25 SS SD"
  override var capac: Long = 102400
  override var spt: Int = 20
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 45
  override var drm: Int = 0x003F
  override var al0: Int = 0x80
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0003
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class OSB2 extends S100HDiskParamsBase {
  override var name: String = "OSB2"
  override var desc: String = "Osborne1 5.25 SS DD"
  override var capac: Long = 204800
  override var spt: Int = 40
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 184
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0003
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class NSSS1 extends S100HDiskParamsBase {
  override var name: String = "NSSS1"
  override var desc: String = "Northstar SSDD Format 1"
  override var capac: Long = 179200
  override var spt: Int = 40
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 0xA4
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0010
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class NSSS2 extends S100HDiskParamsBase {
  override var name: String = "NSSS2"
  override var desc: String = "Northstar SSDD Format 2"
  override var capac: Long = 179200
  override var spt: Int = 40
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 0x51
  override var drm: Int = 0x003F
  override var al0: Int = 0x80
  override var al1: Int = 0x00
  override var cks: Int = 0x0010
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class NSDS2 extends S100HDiskParamsBase {
  override var name: String = "NSDS2"
  override var desc: String = "Northstar DSDD Format 2"
  override var capac: Long = 358400
  override var spt: Int = 40
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 0xA3
  override var drm: Int = 0x003F
  override var al0: Int = 0x80
  override var al1: Int = 0x00
  override var cks: Int = 0x0010
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}

class VGSS extends S100HDiskParamsBase {
  override var name: String = "VGSS"
  override var desc: String = "Vector SS SD"
  override var capac: Long = 315392
  override var spt: Int = 32
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 149
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0020
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class VGDS extends S100HDiskParamsBase {
  override var name: String = "VGDS"
  override var desc: String = "Vector DS SD"
  override var capac: Long = 630784
  override var spt: Int = 32
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 299
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0020
  override var off: Int = 0x0004
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class DISK1A extends S100HDiskParamsBase {
  override var name: String = "DISK1A"
  override var desc: String = "CompuPro Disk1A 8 SS SD"
  override var capac: Long = 630784
  override var spt: Int = 64
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 299
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0020
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class SSSD8 extends S100HDiskParamsBase {
  override var name: String = "SSSD8"
  override var desc: String = "Standard 8 SS SD"
  override var capac: Long = 256256
  override var spt: Int = S100HDSKDevice.SPT26
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 242
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class SSSD8S extends S100HDiskParamsBase {
  override var name: String = "SSSD8S"
  override var desc: String = "Standard 8 SS SD with skew"
  override var capac: Long = 256256
  override var spt: Int = S100HDSKDevice.SPT26
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 242
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(standard8)

}

class SSDD8 extends S100HDiskParamsBase {
  override var name: String =  "SSDD8"
  override var desc: String = "Standard 8 SS DD"
  override var capac: Long = 512512
  override var spt: Int = S100HDSKDevice.SPT52
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 242
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class SSDD8S extends S100HDiskParamsBase {
  override var name: String = "SSDD8S"
  override var desc: String = "Standard 8 SS DD with skew"
  override var capac: Long = 512512
  override var spt: Int = S100HDSKDevice.SPT52
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 242
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(standard8)

}
class DSDD8 extends S100HDiskParamsBase {
  override var name: String = "DSDD8"
  override var desc: String = "Standard 8 DS DD"
  override var capac: Long = 1025024
  override var spt: Int = S100HDSKDevice.SPT52
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 493
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class DSDD8S extends S100HDiskParamsBase {
  override var name: String = "DSDD8S"
  override var desc: String = "Standard 8 DS DD with skew"
  override var capac: Long = 1025024
  override var spt: Int = S100HDSKDevice.SPT52
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 493
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class D512SSDD8 extends S100HDiskParamsBase {
  override var name: String = "512SSDD8"
  override var desc: String = "Standard 8 SS DD with 512 byte sectors"
  override var capac: Long = 591360
  override var spt: Int = 60
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 280
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class D512DSDD8 extends S100HDiskParamsBase {
  override var name: String = "512DSDD8"
  override var desc: String = "Standard 8 DS DD with 512 byte sectors"
  override var capac: Long = 1182720
  override var spt: Int = 60
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 569
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
class APPLEDO extends S100HDiskParamsBase {
  override var name: String = "APPLE-DO"
  override var desc: String = "Apple II DOS 3.3"
  override var capac: Long = 143360
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 127
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0003
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(apple_ii_DOS)

}
class APPLEPO extends S100HDiskParamsBase {
  override var name: String = "APPLE-PO"
  override var desc: String = "Apple II PRODOS"
  override var capac: Long = 143360
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 127
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0003
  override var psh: Int = 0x01
  override var phm: Int = 0x01
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(apple_ii_PRODOS)

}
class APPLED2 extends S100HDiskParamsBase {
  override var name: String = "APPLE-D2"
  override var desc: String = "Apple II DOS 3.3, deblocked"
  override var capac: Long = 143360
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 127
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x003
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(apple_ii_DOS2)

}
class APPLEP2 extends S100HDiskParamsBase {
  override var name: String = "APPLE-P2"
  override var desc: String = "Apple II PRODOS, deblocked"
  override var capac: Long = 143360
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 127
  override var drm: Int = 0x003F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0003
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = Some(apple_ii_PRODOS2)

}
class MITS extends S100HDiskParamsBase {
  override var name: String = "MITS"
  override var desc: String = "MITS Altair original"
  override var capac: Long = 337568
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x03
  override var blm: Int = 0x07
  override var exm: Int = 0x00
  override var dsm: Int = 254
  override var drm: Int = 0x00FF
  override var al0: Int = 0xFF
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0006
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 137
  override var offset: Int = 3
  override var skew: Option[Array[Int]] = Some(mits)

}
class MITS2 extends S100HDiskParamsBase {
  override var name: String = "MITS2"
  override var desc: String = "MITS Altair original, extra"
  override var capac: Long = 1113536
  override var spt: Int = S100HDSKDevice.SPT32
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x00
  override var dsm: Int = 0x1EF
  override var drm: Int = 0x00FF
  override var al0: Int = 0xF0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0006
  override var psh: Int = 0x00
  override var phm: Int = 0x00
  override var physicalSectorSize: Int = 137
  override var offset: Int = 3
  override var skew: Option[Array[Int]] = Some(mits)

}
class V1050 extends S100HDiskParamsBase {
  //
  //dw     40              ;#128 byte records/track
  //db     4,0fh           ;block shift mask (2K)
  //db     1               ;extent  mask
  //dw     194             ;maximum  block number
  //dw     127             ;max number of dir entry - 1
  //db     0C0H,00h        ;alloc vector for directory
  //dw     0020h           ;checksum size
  //dw     2               ;offset for sys tracks
  //db     2,3             ;physical sector shift (512 sector)

  override var name: String =  "V1050"
  override var desc: String = "Visual Technology Visual 1050"
  override var capac: Long = 409600
  override var spt: Int = 40
  override var bsh: Int = 0x04
  override var blm: Int = 0x0F
  override var exm: Int = 0x01
  override var dsm: Int = 194
  override var drm: Int = 0x007F
  override var al0: Int = 0xC0
  override var al1: Int = 0x00
  override var cks: Int = 0x0000
  override var off: Int = 0x0002
  override var psh: Int = 0x02
  override var phm: Int = 0x03
  override var physicalSectorSize: Int = 0
  override var offset: Int = 0
  override var skew: Option[Array[Int]] = None

}
