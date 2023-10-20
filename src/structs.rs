use super::{Error, Result, RW};

macro_rules! int_enum {
    ($name: ident as $type: ty: $($variant: ident = $value: expr,)+) => {
        impl $name {
            pub(super) fn int_value(self) -> $type {
                match self {
                    $(Self::$variant => $value,)+
                    Self::Custom(value) => value,
                }
            }

            pub(super) fn from_int(value: $type) -> Self {
                match value {
                    $($value => Self::$variant,)+
                    value => Self::Custom(value),
                }
            }
        }
    };

    // No custom
    (!$name: ident as $type: ty: $($variant: ident = $value: expr,)+) => {
        impl $name {
            pub(super) fn int_value(self) -> $type {
                match self {
                    $(Self::$variant => $value,)+
                }
            }

            pub(super) fn from_int(value: $type) -> Result<Self> {
                match value {
                    $($value => Ok(Self::$variant),)+
                    value => Err(Error::Error(format!("Invalid value for {}: {value}", stringify!($name)))),
                }
            }
        }
    };
}

// * ------------------------------------ Structs ----------------------------------- * //
/// Class, stored in e_ident. ELF32/ELF64
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Class {
    /// 32 bit ELF file. Adresses are encoded as u32
    ELF32,
    /// 64 bit ELF file. Adresses are encoded as u64
    #[default]
    ELF64,
    /// Custom value
    Custom(u8),
}

int_enum! {
    Class as u8:
    ELF32 = 1,
    ELF64 = 2,
}

/// Byte order, stored in e_ident.
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ByteOrder {
    /// Little Endian
    #[default]
    LSB,
    /// Big endian
    MSB,
}

int_enum! {
    !ByteOrder as u8:
    LSB = 1,
    MSB = 2,
}

#[cfg(feature = "target-lexicon")]
impl From<target_lexicon::Endianness> for ByteOrder {
    fn from(value: target_lexicon::Endianness) -> Self {
        match value {
            target_lexicon::Endianness::Little => Self::LSB,
            target_lexicon::Endianness::Big => Self::MSB,
        }
    }
}

/// ABI. There is planety to chose from, but you probably should just use [ABI::None]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum ABI {
    #[default]
    /// System V
    None,
    /// HP-UX
    HPUX,
    /// NetBSD
    NetBSD,
    /// Linux
    Linux,
    /// GNU Hurd
    Hurd,
    /// Solaris
    Solaris,
    /// AIX (Monterey)
    AIX,
    /// IRIX
    IRIX,
    /// FreeBSD
    FreeBSD,
    /// Tru64
    Tru64,
    /// Novell Modesto
    Modesto,
    /// OpenBSD
    OpenBSD,
    /// OpenVMS
    OpenVMS,
    /// NonStop Kernel
    NSK,
    /// AROS
    AROS,
    /// FenixOS
    FenixOS,
    /// Nuxi CloudABI
    CloudABI,
    /// Stratus Technologies OpenVOS
    OpenVOS,
    /// Custom value
    Custom(u8),
}

int_enum! {
    ABI as u8:
    None = 0x00,
    HPUX = 0x01,
    NetBSD = 0x02,
    Linux = 0x03,
    Hurd = 0x04,
    Solaris = 0x06,
    AIX = 0x07,
    IRIX = 0x08,
    FreeBSD = 0x09,
    Tru64 = 0x0A,
    Modesto = 0x0B,
    OpenBSD = 0x0C,
    OpenVMS = 0x0D,
    NSK = 0x0E,
    AROS = 0x0F,
    FenixOS = 0x10,
    CloudABI = 0x11,
    OpenVOS = 0x12,
}

#[cfg(feature = "target-lexicon")]
impl From<target_lexicon::OperatingSystem> for ABI {
    fn from(value: target_lexicon::OperatingSystem) -> Self {
        match value {
            target_lexicon::OperatingSystem::Unknown => Self::None,
            target_lexicon::OperatingSystem::Aix => Self::AIX,
            target_lexicon::OperatingSystem::Cloudabi => Self::CloudABI,
            target_lexicon::OperatingSystem::Freebsd => Self::FreeBSD,
            target_lexicon::OperatingSystem::Linux => Self::Linux,
            target_lexicon::OperatingSystem::Netbsd => Self::NetBSD,
            target_lexicon::OperatingSystem::None_ => Self::None,
            target_lexicon::OperatingSystem::Openbsd => Self::OpenBSD,
            target_lexicon::OperatingSystem::Solaris => Self::Solaris,
            _ => Self::None,
        }
    }
}

/// Type of the ELF.
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    /// Relocatable file
    Rel,
    /// Executable file
    Exec,
    /// Shared object
    Dyn,
    /// Core file
    Core,
    /// Custom value
    Custom(u16),
}

int_enum! {
    Type as u16:
    Rel = 1,
    Exec = 2,
    Dyn = 3,
    Core = 4,
}

/// Machine. There is A LOT of them. Most common are [Machine::X86], [Machine::X86_64], [Machine::ARM] and [Machine::ARM64]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Machine {
    /// No specific instruction set
    None,
    /// AT&T WE 32100
    M32,
    /// SPARC
    SPARC,
    /// x86, Intel 80386
    X86,
    /// Motorola 68000 (M68k)
    M68k,
    /// Motorola 88000 (M88k)
    M88k,
    /// Intel MCU
    IntelMCU,
    /// Intel 80860
    Intel80860,
    /// MIPS
    MIPS,
    /// IBM System/370
    S370,
    /// MIPS RS3000 Little-endian
    MipsRS3LE,
    /// Hewlett-Packard PA-RISC
    PARISC,
    /// Intel 80960
    Intel80960,
    /// PowerPC
    PowerPC,
    /// PowerPC (64-bit)
    PowerPC64,
    /// S390, including S390x
    S390,
    /// IBM SPU/SPC
    SPU,
    /// NEC V800
    V800,
    /// Fujitsu FR20
    FR20,
    /// TRW RH-32
    RH32,
    /// Motorola RCE
    RCE,
    /// Arm (up to Armv7/AArch32)
    ARM,
    /// Digital Alpha
    DigitalAlpha,
    /// SuperH
    SuperH,
    /// SPARC Version 9
    SPARC9,
    /// Siemens TriCore embedded processor
    TriCore,
    /// Argonaut RISC Core
    ARC,
    /// Hitachi H8/300
    H8_300,
    /// Hitachi H8/300H
    H8_300H,
    /// Hitachi H8S
    H8S,
    /// Hitachi H8/500
    H8_500,
    /// IA-64
    IA64,
    /// Stanford MIPS-X
    MipsX,
    /// Motorola ColdFire
    ColdFire,
    /// Motorola M68HC12
    M68HC12,
    /// Fujitsu MMA Multimedia Accelerator
    MMA,
    /// Siemens PCP
    PCP,
    /// Sony nCPU embedded RISC processor
    NCPU,
    /// Denso NDR1 microprocessor
    NDR1,
    /// Motorola Star*Core processor
    StarCore,
    /// Toyota ME16 processor
    ME16,
    /// STMicroelectronics ST100 processor
    ST100,
    /// Advanced Logic Corp. TinyJ embedded processor family
    TinyJ,
    /// AMD x86-64
    X86_64,
    /// Sony DSP Processor
    SonyDSP,
    /// Digital Equipment Corp. PDP-10
    PDP10,
    /// Digital Equipment Corp. PDP-11
    PDP11,
    /// Siemens FX66 microcontroller
    FX66,
    /// STMicroelectronics ST9+ 8/16 bit microcontroller
    ST9,
    /// STMicroelectronics ST7 8-bit microcontroller
    ST7,
    /// Motorola MC68HC16 Microcontroller
    MC68HC16,
    /// Motorola MC68HC11 Microcontroller
    MC68HC11,
    /// Motorola MC68HC08 Microcontroller
    MC68HC08,
    /// Motorola MC68HC05 Microcontroller
    MC68HC05,
    /// Silicon Graphics SVx
    SVx,
    /// STMicroelectronics ST19 8-bit microcontroller
    ST19,
    /// Digital VAX
    DigitalVAX,
    /// Axis Communications 32-bit embedded processor
    AxisCommunications,
    /// Infineon Technologies 32-bit embedded processor
    InfineonTechnologies,
    /// Element 14 64-bit DSP Processor
    Element14,
    /// LSI Logic 16-bit DSP Processor
    LSILogic,
    /// TMS320C6000 Family
    TMS320C6000,
    /// MCST Elbrus e2k
    MCSTElbrusE2k,
    /// Arm 64-bits (Armv8/AArch64)
    ARM64,
    /// Zilog Z80
    Z80,
    /// RISC-V
    RISCV,
    /// Berkeley Packet Filter
    BerkeleyPacketFilter,
    /// WDC 65C816
    WDC65C816,
    /// Custom value
    Custom(u16),
}

int_enum! {
    Machine as u16:
    None = 0x00,
    M32 = 0x01,
    SPARC = 0x02,
    X86 = 0x03,
    M68k = 0x04,
    M88k = 0x05,
    IntelMCU = 0x06,
    Intel80860 = 0x07,
    MIPS = 0x08,
    S370 = 0x09,
    MipsRS3LE = 0x0A,
    PARISC = 0x0F,
    Intel80960 = 0x13,
    PowerPC = 0x14,
    PowerPC64 = 0x15,
    S390 = 0x16,
    SPU = 0x17,
    V800 = 0x24,
    FR20 = 0x25,
    RH32 = 0x26,
    RCE = 0x27,
    ARM = 0x28,
    DigitalAlpha = 0x29,
    SuperH = 0x2A,
    SPARC9 = 0x2B,
    TriCore = 0x2C,
    ARC = 0x2D,
    H8_300 = 0x2E,
    H8_300H = 0x2F,
    H8S = 0x30,
    H8_500 = 0x31,
    IA64 = 0x32,
    MipsX = 0x33,
    ColdFire = 0x34,
    M68HC12 = 0x35,
    MMA = 0x36,
    PCP = 0x37,
    NCPU = 0x38,
    NDR1 = 0x39,
    StarCore = 0x3A,
    ME16 = 0x3B,
    ST100 = 0x3C,
    TinyJ = 0x3D,
    X86_64 = 0x3E,
    SonyDSP = 0x3F,
    PDP10 = 0x40,
    PDP11 = 0x41,
    FX66 = 0x42,
    ST9 = 0x43,
    ST7 = 0x44,
    MC68HC16 = 0x45,
    MC68HC11 = 0x46,
    MC68HC08 = 0x47,
    MC68HC05 = 0x48,
    SVx = 0x49,
    ST19 = 0x4A,
    DigitalVAX = 0x4B,
    AxisCommunications = 0x4C,
    InfineonTechnologies = 0x4D,
    Element14 = 0x4E,
    LSILogic = 0x4F,
    TMS320C6000 = 0x8C,
    MCSTElbrusE2k = 0xAF,
    ARM64 = 0xB7,
    Z80 = 0xDC,
    RISCV = 0xF3,
    BerkeleyPacketFilter = 0xF7,
    WDC65C816 = 0x101,
}

#[cfg(feature = "target-lexicon")]
impl From<target_lexicon::Architecture> for Machine {
    fn from(value: target_lexicon::Architecture) -> Self {
        match value {
            target_lexicon::Architecture::Unknown => Self::None,
            target_lexicon::Architecture::Arm(_) => Self::ARM,
            target_lexicon::Architecture::Aarch64(_) => Self::ARM64,
            target_lexicon::Architecture::X86_32(_) => Self::X86,
            target_lexicon::Architecture::M68k => Self::M68k,
            target_lexicon::Architecture::Mips32(_) => Self::MIPS,
            target_lexicon::Architecture::Mips64(_) => Self::MIPS,
            target_lexicon::Architecture::Powerpc => Self::PowerPC,
            target_lexicon::Architecture::Powerpc64 => Self::PowerPC64,
            target_lexicon::Architecture::Powerpc64le => Self::PowerPC64,
            target_lexicon::Architecture::Riscv32(_) => Self::RISCV,
            target_lexicon::Architecture::Riscv64(_) => Self::RISCV,
            target_lexicon::Architecture::S390x => Self::S390,
            target_lexicon::Architecture::Sparc => Self::SPARC,
            target_lexicon::Architecture::Sparc64 => Self::SPARC9,
            target_lexicon::Architecture::Sparcv9 => Self::SPARC9,
            target_lexicon::Architecture::X86_64 => Self::X86_64,
            target_lexicon::Architecture::X86_64h => Self::X86_64,
            _ => todo!(),
        }
    }
}

/// ELF is supposed to be 32/64. This is a trait to specify this. It's implemented for [u32] and [u64].
pub trait SizeT: Sized + RW {
    /// Construct a new value
    fn new(value: u64) -> Self;

    /// Get a value as a [u64]
    fn value(&self) -> u64;

    /// A class for this type, is it for ELF32 or ELF64
    const CLASS: Class;

    /// Header size, bigger for ELF64
    const ELF_HEADER_SIZE: u16;

    /// Segment header size, bigger for ELF64
    const SEGMENT_HEADER_SIZE: u16;

    /// Section header size, bigger for ELF64
    const SECTION_HEADER_SIZE: u16;

    /// For some reason, for ELF64 p_flags are moved
    const MOVE_PFLAGS: bool;
}

// * Segment

/// p_type. A segment type (type of a program header). Most segments will probably have type [SegmentType::Load]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SegmentType {
    /// Program header table entry unused.
    None,
    /// Loadable segment.
    Load,
    /// Dynamic linking information.
    Dynamic,
    /// Interpreter information.
    Interpreter,
    /// Auxiliary information.
    Note,
    /// Reserved.
    ShLib,
    /// Segment containing program header table itself.
    Phdr,
    /// Thread-Local Storage template.
    Tls,
    /// Custom value
    Custom(u32),
}

int_enum! {
    SegmentType as u32:
    None = 0x00,
    Load = 0x01,
    Dynamic = 0x02,
    Interpreter = 0x03,
    Note = 0x04,
    ShLib = 0x05,
    Phdr = 0x06,
    Tls = 0x07,
}

/// p_flags. Essentially permissions. See [`Segment::flags`] for details
#[repr(u32)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum SegmentFlags {
    ///  Executable segment
    Executable = 0x01,
    ///  Writeable segment
    Writeable = 0x02,
    ///  Readable segment
    Readable = 0x04,
}
