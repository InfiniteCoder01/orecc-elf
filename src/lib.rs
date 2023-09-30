//! Easy read/write ELF 32/64 relocatibles/executables/dynamics
//!
//! To read an elf file:
//! ```
//! let mut file = std::fs::File::open("test.o").unwrap();
//! dbg!(orecc_elf::ELF::<u64>::read(&mut file));
//! ```
//!
//! To write an elf file:
//! ```
//! let mut file = std::fs::File::create("test.o").unwrap();
//! orecc_elf::ELF::new(
//!     orecc_elf::Ident::default(),
//!     orecc_elf::Type::Exec,
//!     orecc_elf::Machine::X86_64,
//!     0xDEADBEEF_u64,
//! )
//! .unwrap()
//! .write(file)
//! .unwrap();
//! ```

use int_enum::IntEnum;
mod serde;
use serde::*;

// * ------------------------------------ Structs ----------------------------------- * //
/// Class, stored in e_ident. ELF32/ELF64
#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Class {
    /// 32 bit ELF file. Adresses are encoded as u32.
    ELF32 = 1,
    /// 64 bit ELF file. Adresses are encoded as u64.
    #[default]
    ELF64 = 2,
}

/// Byte order, stored in e_ident.
#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum ByteOrder {
    /// Little Endian
    #[default]
    LSB = 1,
    /// Big endian
    MSB = 2,
}

/// ABI. There is planety to chose from, but you probably should just use [ABI::None]
#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum ABI {
    #[default]
    None = 0,
    HPUX = 1,
    NetBSD = 2,
    Linux = 3,
    Solaris = 4,
    AIX = 5,
    IRIX = 6,
    FreeBSD = 7,
    Tru64 = 8,
    Modesto = 9,
    OpenBSD = 10,
    OpenVMS = 11,
    NSK = 12,
    AROS = 13,
    FenixOS = 14,
    CloudABI = 15,
    OpenVOS = 16,
}

/// Type of the ELF.
#[repr(u16)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Type {
    /// Relocatable file
    Rel = 1,
    /// Executable file
    Exec = 2,
    /// Shared object
    Dyn = 3,
    /// Core file
    Core = 4,
}

/// Machine. There is A LOT of them. Most common are [Machine::X86], [Machine::X86_64], [Machine::ARM] and [Machine::ARM64]
#[repr(u16)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Machine {
    /// No specific instruction set
    None = 0x00,
    /// AT&T WE 32100
    M32 = 0x01,
    /// SPARC
    SPARC = 0x02,
    /// x86
    X86 = 0x03,
    /// Motorola 68000 (M68k)
    M68k = 0x04,
    /// Motorola 88000 (M88k)
    M88k = 0x05,
    /// Intel MCU
    IntelMCU = 0x06,
    /// Intel 80860
    Intel80860 = 0x07,
    /// MIPS
    MIPS = 0x08,
    /// IBM System/370
    S370 = 0x09,
    /// MIPS RS3000 Little-endian
    MipsRS3LE = 0x0A,
    /// Hewlett-Packard PA-RISC
    PARISC = 0x0F,
    /// Intel 80960
    Intel80960 = 0x13,
    /// PowerPC
    PowerPC = 0x14,
    /// PowerPC (64-bit)
    PowerPC64 = 0x15,
    /// S390, including S390x
    S390 = 0x16,
    /// IBM SPU/SPC
    SPU = 0x17,
    /// NEC V800
    V800 = 0x24,
    /// Fujitsu FR20
    FR20 = 0x25,
    /// TRW RH-32
    RH32 = 0x26,
    /// Motorola RCE
    RCE = 0x27,
    /// Arm (up to Armv7/AArch32)
    ARM = 0x28,
    /// Digital Alpha
    DigitalAlpha = 0x29,
    /// SuperH
    SuperH = 0x2A,
    /// SPARC Version 9
    SPARC9 = 0x2B,
    /// Siemens TriCore embedded processor
    TriCore = 0x2C,
    /// Argonaut RISC Core
    ARC = 0x2D,
    /// Hitachi H8/300
    H8_300 = 0x2E,
    /// Hitachi H8/300H
    H8_300H = 0x2F,
    /// Hitachi H8S
    H8S = 0x30,
    /// Hitachi H8/500
    H8_500 = 0x31,
    /// IA-64
    IA64 = 0x32,
    /// Stanford MIPS-X
    MipsX = 0x33,
    /// Motorola ColdFire
    ColdFire = 0x34,
    /// Motorola M68HC12
    M68HC12 = 0x35,
    /// Fujitsu MMA Multimedia Accelerator
    MMA = 0x36,
    /// Siemens PCP
    PCP = 0x37,
    /// Sony nCPU embedded RISC processor
    NCPU = 0x38,
    /// Denso NDR1 microprocessor
    NDR1 = 0x39,
    /// Motorola Star*Core processor
    StarCore = 0x3A,
    /// Toyota ME16 processor
    ME16 = 0x3B,
    /// STMicroelectronics ST100 processor
    ST100 = 0x3C,
    /// Advanced Logic Corp. TinyJ embedded processor family
    TinyJ = 0x3D,
    /// AMD x86-64
    X86_64 = 0x3E,
    /// Sony DSP Processor
    SonyDSP = 0x3F,
    /// Digital Equipment Corp. PDP-10
    PDP10 = 0x40,
    /// Digital Equipment Corp. PDP-11
    PDP11 = 0x41,
    /// Siemens FX66 microcontroller
    FX66 = 0x42,
    /// STMicroelectronics ST9+ 8/16 bit microcontroller
    ST9 = 0x43,
    /// STMicroelectronics ST7 8-bit microcontroller
    ST7 = 0x44,
    /// Motorola MC68HC16 Microcontroller
    MC68HC16 = 0x45,
    /// Motorola MC68HC11 Microcontroller
    MC68HC11 = 0x46,
    /// Motorola MC68HC08 Microcontroller
    MC68HC08 = 0x47,
    /// Motorola MC68HC05 Microcontroller
    MC68HC05 = 0x48,
    /// Silicon Graphics SVx
    SVx = 0x49,
    /// STMicroelectronics ST19 8-bit microcontroller
    ST19 = 0x4A,
    /// Digital VAX
    DigitalVAX = 0x4B,
    /// Axis Communications 32-bit embedded processor
    AxisCommunications = 0x4C,
    /// Infineon Technologies 32-bit embedded processor
    InfineonTechnologies = 0x4D,
    /// Element 14 64-bit DSP Processor
    Element14 = 0x4E,
    /// LSI Logic 16-bit DSP Processor
    LSILogic = 0x4F,
    /// TMS320C6000 Family
    TMS320C6000 = 0x8C,
    /// MCST Elbrus e2k
    MCSTElbrusE2k = 0xAF,
    /// Arm 64-bits (Armv8/AArch64)
    ARM64 = 0xB7,
    /// Zilog Z80
    Z80 = 0xDC,
    /// RISC-V
    RISCV = 0xF3,
    /// Berkeley Packet Filter
    BerkeleyPacketFilter = 0xF7,
    /// WDC 65C816
    WDC65C816 = 0x101,
}

/// ELF is supposed to be 32/64. This is a trait to specify this. It's implemented for [u32] and [u64].
pub trait SizeT: Sized + RW {
    fn class() -> Class;
}

// * ------------------------------------ E_IDENT ----------------------------------- * //
/// e_ident. Specifies class, byte order and ABI of the ELF
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    pub class: Class,
    pub byte_order: ByteOrder,
    pub abi: ABI,
    pub abi_version: u8,
}

impl Ident {
    /// Constructs a new Ident to write it afterwards
    pub fn new(class: Class, byte_order: ByteOrder, abi: ABI, abi_version: u8) -> Self {
        Self {
            class,
            byte_order,
            abi,
            abi_version,
        }
    }

    /// Write the Ident to a file. It's done automatically by the [`ELF::write()`]
    #[rustfmt::skip]
    pub fn write<W: std::io::Write>(&self, file: &mut W) -> Result<()> {
        file.write_all(&[
            0x7f, 0x45, 0x4c, 0x46,
            self.class as u8, self.byte_order as u8, 1,
            self.abi as u8, self.abi_version,
            0, 0, 0, 0, 0, 0, 0,
        ]).map_err(|err|Error::io(err, "e_ident"))
    }

    /// Read the Ident from a file. It's done automatically by the [`ELF::read()`].
    /// Can be used with [`ELF::read_reminder()`] to determine ELF class:
    /// ```
    /// use orecc_elf::{ELF, Ident, Class};
    ///
    /// let mut file = std::fs::File::open("test.o").unwrap();
    /// let ident = Ident::read(&mut file).unwrap();
    /// if ident.class == Class::ELF64 {
    ///     dbg!(ELF::<u64>::read(&mut file)).unwrap();
    /// } else {
    ///     dbg!(ELF::<u32>::read(&mut file)).unwrap();
    /// }
    /// ```
    pub fn read<R: std::io::Read>(file: &mut R) -> Result<Self> {
        let mut buffer = [0_u8; 4];
        file.read_exact(&mut buffer).map_err(|err| {
            Error::Signature(format!("failed to read ELF signature from a file: {err}"))
        })?;
        if buffer != [0x7f, 0x45, 0x4c, 0x46] {
            return Err(Error::Signature(format!(
                "incorrect ELF signature: {buffer:02X?}, expected: [0x7f, 0x45, 0x4c, 0x46]"
            )));
        }
        let mut buffer = [0_u8; 12];
        file.read_exact(&mut buffer)
            .map_err(|err| Error::io(err, "e_ident"))?;
        let [class, byte_order, version, abi, abi_version] = buffer[..5] else {
            panic!("Internal error: buffer size is not compatible with e_ident size!");
        };
        if version != 1 {
            return Err(Error::parse("version should be always 1", "version"));
        }

        let class = Class::from_int(class).map_err(|err| Error::parse(err, "class"))?;
        let byte_order = ByteOrder::from_int(byte_order)
            .map_err(|err| Error::parse(err, "data (byte_order)"))?;
        let abi = ABI::from_int(abi).map_err(|err| Error::parse(err, "abi"))?;
        Ok(Self {
            class,
            byte_order,
            abi,
            abi_version,
        })
    }
}

// * ------------------------------------ Header ------------------------------------ * //
/// The ELF file itself. This what you will be using.
/// Use [`Self::read()`] to read if from a file,
/// [`Self::new()`] to construct it from scratch
/// and [`Self::write()`] to write it to a file
/// `T` generic can be [u32] for ELF32 and [u64] for ELF64
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ELF<T: SizeT> {
    pub ident: Ident,
    pub e_type: Type,
    pub machine: Machine,
    pub entry_point: T,
}

macro_rules! rw_enum {
    (read $type: ident, $name: ident, $display_part: ident, $storage: ident, $ident: ident, $file: ident) => {
        $type::from_int($storage::read(
            $file,
            $ident.byte_order,
            stringify!($display_part),
        )?)
        .map_err(|err| Error::parse(err, stringify!($display_part)))?
    };

    (write $self: ident, $part: ident, $file: ident) => {
        $self
            .$part
            .int_value()
            .write($file, $self.ident.byte_order, stringify!($part))?
    };
}

impl<T: SizeT> ELF<T> {
    /// Constructs a new ELF from scratch. To make an [Ident] you will need to call [`Ident::new()`]
    pub fn new(ident: Ident, e_type: Type, machine: Machine, entry_point: T) -> Result<Self> {
        if ident.class != T::class() {
            return Err(Error::Error(format!(
                "Expected ELF class {:?}, got class {:?}",
                T::class(),
                ident.class
            )));
        }

        Ok(Self {
            ident,
            e_type,
            machine,
            entry_point,
        })
    }

    /// Write an ELF to a file
    pub fn write<W: std::io::Write>(&self, file: &mut W) -> Result<()> {
        self.ident.write(file)?;
        rw_enum!(write self, e_type, file);
        rw_enum!(write self, machine, file);
        1_u32.write(file, self.ident.byte_order, "e_version")?;
        self.entry_point
            .write(file, self.ident.byte_order, "e_entry")?;
        Ok(())
    }

    /// Read an ELF from a file
    pub fn read<R: std::io::Read>(file: &mut R) -> Result<Self> {
        let ident = Ident::read(file)?;
        if ident.class != T::class() {
            return Err(Error::Error(format!(
                "Expected ELF class {:?}, got class {:?}",
                T::class(),
                ident.class
            )));
        }

        Self::read_reminder(file, ident)
    }

    /// Read an ELF from a file when you already read [Ident]
    pub fn read_reminder<R: std::io::Read>(file: &mut R, ident: Ident) -> Result<Self> {
        let e_type = rw_enum!(read Type, e_type, e_type, u16, ident, file);
        let machine = rw_enum!(read Machine, machine, e_machine, u16, ident, file);
        if u32::read(file, ident.byte_order, "e_version")? != 1 {
            return Err(Error::parse("ELF version should be always 1", "e_version"));
        }
        let entry_point = T::read(file, ident.byte_order, "e_entry")?;
        Ok(Self {
            ident,
            e_type,
            machine,
            entry_point,
        })
    }
}

// * ------------------------------------- Size ------------------------------------- * //
impl SizeT for u32 {
    fn class() -> Class {
        Class::ELF32
    }
}

impl SizeT for u64 {
    fn class() -> Class {
        Class::ELF64
    }
}

// pub fn pack<W: std::io::Write>(file: &mut W, data: &[u8]) -> std::io::Result<()> {
//     let class = 0x2;
//     let byte_order = 0x1;
//     let abi = 0x0;
//     let abi_version = 0x0;

//     file.write_all(&[0x7f, 0x45, 0x4c, 0x46])?; // Magic
//     #[rustfmt::skip]
//     file.write_all(&[class, byte_order, 0x1, abi, abi_version, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0])?;
//     elf64_header(file)?;
//     Ok(())
// }

// pub fn elf64<W: std::io::Write>(file: &mut W) -> std::io::Result<()> {
//     let program_header_entry_size: u16 = 56;
//     let section_header_entry_size: u16 = 64;

//     let elf_type: u16 = 0x2;
//     let machine: u16 = 0x3;
//     let entry_point: u64 = 0; // .
//     let program_header_offset: u64 = 0; // .
//     let section_header_offset: u64 = 0; // .
//     let flags: u32 = 0;
//     let string_table_index: u16 = 0;

//     file.write_all(&elf_type.to_le_bytes())?;
//     file.write_all(&machine.to_le_bytes())?;
//     file.write_all(&1_u32.to_le_bytes())?;
//     file.write_all(&entry_point.to_le_bytes())?;
//     file.write_all(&program_header_offset.to_le_bytes())?;
//     file.write_all(&section_header_offset.to_le_bytes())?;
//     file.write_all(&flags.to_le_bytes())?;
//     file.write_all(&64_u16.to_le_bytes())?;
//     file.write_all(&program_header_entry_size.to_le_bytes())?;
//     file.write_all(&0_u16.to_le_bytes())?; // e_phnum
//     file.write_all(&section_header_entry_size.to_le_bytes())?;
//     file.write_all(&0_u16.to_le_bytes())?; // e_shnum
//     file.write_all(&string_table_index.to_le_bytes())?;
//     Ok(())
// }
// * ------------------------------------ Errors ------------------------------------ * //
/// A custom result type
pub type Result<T> = std::result::Result<T, Error>;

/// A custom error type
#[derive(Clone, Debug)]
pub enum Error {
    /// The file you reading is not an ELF file
    Signature(String),
    /// The ELF file is corrupted
    Error(String),
}

impl Error {
    fn io(err: std::io::Error, part: &str) -> Self {
        Self::Error(format!("failed to read/write {part} to/from a file: {err}"))
    }

    fn parse(err: impl ToString, part: &str) -> Self {
        Self::Error(format!("failed to parse {part}: {}", err.to_string()))
    }
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Signature(msg) => f.write_str(msg),
            Error::Error(msg) => f.write_str(msg),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // #[test]
    // fn it_works() {
    //     let result = add(2, 2);
    //     assert_eq!(result, 4);
    // }
}
