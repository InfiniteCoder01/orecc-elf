//! Easy read/write ELF 32/64 relocatibles/executables/dynamics
//!
//! To read an elf file:
//! ```
//! let mut file = std::fs::File::open("test.o").unwrap();
//! dbg!(orecc_elf::ELF::<u64>::read(&mut file));
//! ```
//!
//! To write an elf file (source: https://www.youtube.com/watch?v=XH6jDiKxod8):
//! ```
//! let mut file = std::fs::File::create("test.o").unwrap();
//! // An x86 program that just exits
//! let data = [
//!     0xB8, 0x01, 0x00, 0x00, 0x00,
//!     0xBB, 0x00, 0x00, 0x00, 0x00,
//!     0xCD, 0x80,
//! ];
//! orecc_elf::ELF::<u32>::new(
//!     orecc_elf::Ident::new(
//!         orecc_elf::Class::ELF32,
//!         orecc_elf::ByteOrder::LSB,
//!         orecc_elf::ABI::None,
//!         0,
//!     ),
//!     orecc_elf::Type::Exec,
//!     orecc_elf::Machine::X86,
//!     true,
//!     vec![
//!         orecc_elf::SegmentTemplate::new(
//!             orecc_elf::SegmentType::Load,
//!             data.to_vec(),
//!             data.len() as _,
//!             orecc_elf::SegmentFlags::Readable as u32 | orecc_elf::SegmentFlags::Executable as u32,
//!         )
//!     ],
//!     Vec::new(),
//! )
//! .unwrap()
//! .write(&mut file)
//! .unwrap();
//! ```
//!
//! x86_64 version:
//! ```
//! let mut file = std::fs::File::create("test.o").unwrap();
//! // An x86 program that just exits
//! let data = [
//!     0x48, 0xC7, 0xC0, 0x3C, 0x00, 0x00, 0x00,
//!     0x48, 0xC7, 0xC7, 0x2A, 0x00, 0x00, 0x00,
//!     0x0F, 0x05,
//! ];
//! orecc_elf::ELF::<u64>::new(
//!     orecc_elf::Ident::default(),
//!     orecc_elf::Type::Exec,
//!     orecc_elf::Machine::X86_64,
//!     true,
//!     vec![
//!         orecc_elf::SegmentTemplate::new(
//!             orecc_elf::SegmentType::Load,
//!             data.to_vec(),
//!             data.len() as _,
//!             orecc_elf::SegmentFlags::Readable as u32 | orecc_elf::SegmentFlags::Executable as u32,
//!         )
//!     ],
//!     Vec::new(),
//! )
//! .unwrap()
//! .write(&mut file)
//! .unwrap();
//! ```

mod serde;
use serde::*;

macro_rules! int_enum {
    ($name: ident as $type: ty: $($variant: ident = $value: expr,)+) => {
        impl $name {
            fn int_value(self) -> $type {
                match self {
                    $(Self::$variant => $value,)+
                    Self::Custom(value) => value,
                }
            }

            fn from_int(value: $type) -> Self {
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
            fn int_value(self) -> $type {
                match self {
                    $(Self::$variant => $value,)+
                }
            }

            fn from_int(value: $type) -> Result<Self> {
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
    /// x86
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

/// ELF is supposed to be 32/64. This is a trait to specify this. It's implemented for [u32] and [u64].
pub trait SizeT: Sized + RW {
    fn new(value: u64) -> Self;
    fn value(&self) -> u64;
    const CLASS: Class;
    const ELF_HEADER_SIZE: u16;
    const SEGMENT_HEADER_SIZE: u16;
    const SECTION_HEADER_SIZE: u16;
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

// * ------------------------------------ E_IDENT ----------------------------------- * //
/// e_ident. Specifies class, byte order and ABI of the ELF
#[derive(Clone, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    /// Class, ELF32/ELF64
    pub class: Class,
    /// Byte order
    pub byte_order: ByteOrder,
    /// Identifies the target operating system ABI
    pub abi: ABI,
    /// ABI version, usually 0
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
            self.class.int_value(), self.byte_order.int_value(), 1,
            self.abi.int_value(), self.abi_version,
            0, 0, 0, 0, 0, 0, 0,
        ]).map_err(|err|Error::io(err, "e_ident"))
    }

    /// Read the Ident from a file. It's done automatically by the [`ELF::read()`].
    /// Can be used with [`ELF::read_remainder()`] to determine ELF class:
    /// ```
    /// use orecc_elf::{ELF, Ident, Class};
    ///
    /// let mut file = std::fs::File::open("test.o").unwrap();
    /// let ident = Ident::read(&mut file).unwrap();
    /// if ident.class == Class::ELF64 {
    ///     dbg!(ELF::<u64>::read_remainder(&mut file, ident)).unwrap();
    /// } else {
    ///     dbg!(ELF::<u32>::read_remainder(&mut file, ident)).unwrap();
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

        let class = Class::from_int(class);
        let byte_order = ByteOrder::from_int(byte_order)?;
        let abi = ABI::from_int(abi);
        Ok(Self {
            class,
            byte_order,
            abi,
            abi_version,
        })
    }
}
/// A segment template, [Segment] will be generated from it
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct SegmentTemplate<T: SizeT> {
    /// Identifies the type of the segment
    pub p_type: SegmentType,
    /// Data of this segment
    pub data: Vec<u8>,
    /// Size in bytes of the segment in memory. May be 0. May be more then data to pad segment with zeros
    pub size: T,
    /// Segment-dependent flags. Essentially permissions, specified with [SegmentFlags] like that:
    /// ```
    /// use orecc_elf::SegmentFlags;
    /// let flags = SegmentFlags::Readable as u32 | SegmentFlags::Executable as u32;
    /// assert_eq!(flags, 0x05);
    /// ```
    pub flags: u32,
}

impl<T: SizeT> SegmentTemplate<T> {
    /// Create a new segment template. Used with [`ELF::new()`]
    pub fn new(p_type: SegmentType, data: Vec<u8>, size: T, flags: u32) -> Self {
        Self {
            p_type,
            data,
            size,
            flags,
        }
    }
}

// * ------------------------------------ Header ------------------------------------ * //
impl Machine {
    fn text_region_address(self) -> u64 {
        match self {
            Self::X86 => 0x08048000,
            Self::X86_64 => 0x400000,
            _ => 100, // TODO: not tested
        }
    }
}

/// The ELF file itself. This what you will be using.
/// Use [`Self::read()`] to read if from a file,
/// [`Self::new()`] to construct it from scratch
/// and [`Self::write()`] to write it to a file
/// `T` generic can be [u32] for ELF32 and [u64] for ELF64
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ELF<T: SizeT> {
    /// An ident
    pub ident: Ident,
    /// Identifies object file type
    pub e_type: Type,
    /// Specifies target instruction set architecture
    pub machine: Machine,
    /// This is the memory address of the entry point from where the process starts executing. Should be 0 if no entry point
    pub entry_point: T,
    /// Segments/Program headers (Loaded when executed). Do not add them, offsets will probably mess up
    pub segments: Vec<Segment<T>>,
    /// Sections (When linking turned into segment). Do not add them, offsets will probably mess up
    pub sections: Vec<Section<T>>,
    /// Flags, usually 0. Interpretation of this field depends on the target architecture
    pub flags: u32,
    /// Contains index of the section header table entry that contains the section names
    pub section_header_string_table_index: u16,
}

macro_rules! rw_enum {
    (read $type: ident, $name: ident, $storage: ident, $ident: ident, $file: ident) => {
        $type::from_int($storage::read(
            $file,
            $ident.byte_order,
            stringify!($display_part),
        )?)
    };

    (write $self: ident, $part: ident, $file: ident) => {
        $self
            .$part
            .int_value()
            .write($file, $self.ident.byte_order, stringify!($part))?
    };
}

impl<T: SizeT> ELF<T> {
    /// Constructs a new ELF from scratch.
    /// To make an [Ident] you will need to call [`Ident::new()`]
    /// This is a simplified constructor:
    /// - Assumes, that string table is the last section (if exists)
    /// - Sets flags to 0
    /// - If has_entry_point is true, entry point will be set to the start of the first segment
    /// For more flexability you can do:
    /// ```
    /// use orecc_elf::*;
    /// dbg!(
    ///     ELF::<u64> {
    ///         ident: Ident::default(),
    ///         e_type: Type::Exec,
    ///         machine: Machine::X86_64,
    ///         entry_point: 0xDEADBEEF,
    ///         segments: Vec::new(),
    ///         sections: Vec::new(),
    ///         flags: 0,
    ///         section_header_string_table_index: 0,
    ///     }
    /// );
    /// ```
    pub fn new(
        ident: Ident,
        e_type: Type,
        machine: Machine,
        has_entry_point: bool,
        segments: Vec<SegmentTemplate<T>>,
        sections: Vec<Section<T>>,
    ) -> Result<Self> {
        if ident.class != T::CLASS {
            return Err(Error::Error(format!(
                "Expected ELF class {:?}, got class {:?}",
                T::CLASS,
                ident.class
            )));
        }

        let section_header_string_table_index = sections.len().saturating_sub(1) as _;

        let segments = {
            let align = 0x1000;

            let mut offset = T::ELF_HEADER_SIZE as u64
                + T::SEGMENT_HEADER_SIZE as u64 * segments.len() as u64
                + T::SECTION_HEADER_SIZE as u64 * sections.len() as u64;
            let mut virtual_address = machine.text_region_address() + (offset % align);

            segments
                .into_iter()
                .map(|segment| {
                    let segment_offset = offset;
                    let segment_virtual_address = virtual_address;
                    offset += segment.data.len() as u64;
                    virtual_address += segment.size.value();
                    Segment {
                        p_type: segment.p_type,
                        data: segment.data,
                        offset: T::new(segment_offset),
                        virtual_address: T::new(segment_virtual_address),
                        physical_address: T::new(0),
                        size: segment.size,
                        flags: segment.flags,
                        align: T::new(align),
                    }
                })
                .collect::<Vec<_>>()
        };

        Ok(Self {
            ident,
            e_type,
            machine,
            entry_point: if has_entry_point {
                T::new(
                    segments
                        .get(0)
                        .ok_or(Error::Error(String::from(
                            "Can't have an entry point with no segments",
                        )))?
                        .virtual_address
                        .value(),
                )
            } else {
                T::new(0)
            },
            segments,
            sections,
            flags: 0,
            section_header_string_table_index,
        })
    }

    /// Write an ELF to a file
    pub fn write<W: std::io::Write + std::io::Seek>(&self, file: &mut W) -> Result<()> {
        let segment_headers_offset = T::ELF_HEADER_SIZE as u64;
        let section_headers_offset =
            segment_headers_offset + T::SEGMENT_HEADER_SIZE as u64 * self.segments.len() as u64;

        // * Ident
        self.ident.write(file)?;
        rw_enum!(write self, e_type, file);
        rw_enum!(write self, machine, file);
        1_u32.write(file, self.ident.byte_order, "e_version")?;
        self.entry_point
            .write(file, self.ident.byte_order, "e_entry")?;

        // * Offsets
        T::new(if self.segments.is_empty() {
            0
        } else {
            segment_headers_offset
        })
        .write(file, self.ident.byte_order, "e_phoff")?;
        T::new(if self.sections.is_empty() {
            0
        } else {
            section_headers_offset
        })
        .write(file, self.ident.byte_order, "e_shoff")?;

        // * Flags
        self.flags.write(file, self.ident.byte_order, "e_flags")?;
        T::ELF_HEADER_SIZE.write(file, self.ident.byte_order, "e_ehsize")?;

        // * Segments
        T::SEGMENT_HEADER_SIZE.write(file, self.ident.byte_order, "e_phentsize")?;
        (self.segments.len() as u16).write(file, self.ident.byte_order, "e_phnum")?;

        // * Sections
        T::SECTION_HEADER_SIZE.write(file, self.ident.byte_order, "e_shentsize")?;
        (self.sections.len() as u16).write(file, self.ident.byte_order, "e_shnum")?;

        self.section_header_string_table_index
            .write(file, self.ident.byte_order, "e_shstrndx")?;

        // * Segment headers
        let mut cursor =
            section_headers_offset + T::SECTION_HEADER_SIZE as u64 * self.sections.len() as u64;
        for segment in &self.segments {
            segment.write_header(file, &mut cursor, self.ident.byte_order)?;
        }

        // * Segment data
        for segment in &self.segments {
            file.seek(std::io::SeekFrom::Start(segment.offset.value()))
                .map_err(|err| Error::io(err, "segment data"))?;
            file.write_all(&segment.data)
                .map_err(|err| Error::io(err, "segment data"))?;
        }

        Ok(())
    }

    /// Read an ELF from a file
    pub fn read<R: std::io::Read + std::io::Seek>(file: &mut R) -> Result<Self> {
        let ident = Ident::read(file)?;
        if ident.class != T::CLASS {
            return Err(Error::Error(format!(
                "Expected ELF class {:?}, got class {:?}",
                T::CLASS,
                ident.class
            )));
        }

        Self::read_remainder(file, ident)
    }

    /// Read an ELF from a file when you already read [Ident]
    pub fn read_remainder<R: std::io::Read>(file: &mut R, ident: Ident) -> Result<Self> {
        let e_type = rw_enum!(read Type, e_type, u16, ident, file);
        let machine = rw_enum!(read Machine, machine, u16, ident, file);
        if u32::read(file, ident.byte_order, "e_version")? != 1 {
            return Err(Error::parse("ELF version should be always 1", "e_version"));
        }
        let entry_point = T::read(file, ident.byte_order, "e_entry")?;

        let program_headers_offset = T::read(file, ident.byte_order, "e_phoff")?;
        let section_headers_offset = T::read(file, ident.byte_order, "e_shoff")?;

        let flags = u32::read(file, ident.byte_order, "e_flags")?;
        let elf_header_size = u16::read(file, ident.byte_order, "e_ehsize")?;
        if elf_header_size != T::ELF_HEADER_SIZE {
            return Err(Error::Error(format!(
                "Invalid elf header size, expected {}, got {elf_header_size}!",
                T::ELF_HEADER_SIZE
            )));
        }

        let segment_header_size = u16::read(file, ident.byte_order, "e_phentsize")?;
        if segment_header_size != T::SEGMENT_HEADER_SIZE {
            return Err(Error::Error(format!(
                "Invalid segment header size, expected {}, got {segment_header_size}!",
                T::SEGMENT_HEADER_SIZE
            )));
        }
        let num_segments = u16::read(file, ident.byte_order, "e_phnum")?;

        let section_header_size = u16::read(file, ident.byte_order, "e_shentsize")?;
        if section_header_size != T::SECTION_HEADER_SIZE {
            return Err(Error::Error(format!(
                "Invalid section header size, expected {}, got {section_header_size}!",
                T::SECTION_HEADER_SIZE
            )));
        }
        let num_sections = u16::read(file, ident.byte_order, "e_shnum")?;

        let section_header_string_table_index = u16::read(file, ident.byte_order, "e_shstrndx")?;

        Ok(Self {
            ident,
            e_type,
            machine,
            entry_point,
            flags,
            segments: Vec::new(),
            sections: Vec::new(),
            section_header_string_table_index,
        })
    }
}

// * ----------------------------------- Segments ----------------------------------- * //
/// A segment, this gets loaded into memory when elf file gets executed
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Segment<T: SizeT> {
    /// Identifies the type of the segment
    pub p_type: SegmentType,
    /// Data of this segment
    pub data: Vec<u8>,
    /// Offset of the segment in the file image
    pub offset: T,
    /// Virtual address of the segment in memory
    pub virtual_address: T,
    /// On systems where physical address is relevant, reserved for segment's physical address
    pub physical_address: T,
    /// Size in bytes of the segment in memory. May be 0. May be more then data to pad segment with zeros
    pub size: T,
    /// Segment-dependent flags. Essentially permissions, specified with [SegmentFlags] like that:
    /// ```
    /// use orecc_elf::SegmentFlags;
    /// let flags = SegmentFlags::Readable as u32 | SegmentFlags::Executable as u32;
    /// assert_eq!(flags, 0x05);
    /// ```
    pub flags: u32,
    /// 0 and 1 specify no alignment. Otherwise should be a positive, integral power of 2. I don't really know how it works
    pub align: T,
}

impl<T: SizeT> Segment<T> {
    fn write_header<W: std::io::Write>(
        &self,
        file: &mut W,
        cursor: &mut u64,
        byte_order: ByteOrder,
    ) -> Result<()> {
        self.p_type.int_value().write(file, byte_order, "p_type")?;
        if T::MOVE_PFLAGS {
            self.flags.write(file, byte_order, "p_flags")?;
        }
        T::new(*cursor).write(file, byte_order, "p_offset")?;
        self.virtual_address.write(file, byte_order, "p_vaddr")?;
        self.physical_address.write(file, byte_order, "p_paddr")?;
        T::new(self.data.len() as _).write(file, byte_order, "p_filesz")?;
        self.size.write(file, byte_order, "p_memsz")?;
        if !T::MOVE_PFLAGS {
            self.flags.write(file, byte_order, "p_flags")?;
        }
        self.align.write(file, byte_order, "p_align")?;
        *cursor += self.data.len() as u64;
        Ok(())
    }
}

// * ----------------------------------- Sections ----------------------------------- * //
/// Section, when linking turned into segment
#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Section<T: SizeT> {
    __: T,
}

// * ------------------------------------- Size ------------------------------------- * //
impl SizeT for u32 {
    fn new(value: u64) -> Self {
        value as _
    }

    fn value(&self) -> u64 {
        *self as _
    }

    const CLASS: Class = Class::ELF32;
    const ELF_HEADER_SIZE: u16 = 52;
    const SEGMENT_HEADER_SIZE: u16 = 32;
    const SECTION_HEADER_SIZE: u16 = 40;
    const MOVE_PFLAGS: bool = false;
}

impl SizeT for u64 {
    fn new(value: u64) -> Self {
        value
    }

    fn value(&self) -> u64 {
        *self
    }

    const CLASS: Class = Class::ELF64;
    const ELF_HEADER_SIZE: u16 = 64;
    const SEGMENT_HEADER_SIZE: u16 = 56;
    const SECTION_HEADER_SIZE: u16 = 64;
    const MOVE_PFLAGS: bool = true;
}

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
