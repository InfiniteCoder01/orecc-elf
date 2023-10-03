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
//! // An x86_64 program that just exits
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

mod structs;
pub use structs::*;

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
    pub fn new(
        class: Class,
        byte_order: impl Into<ByteOrder>,
        abi: impl Into<ABI>,
        abi_version: u8,
    ) -> Self {
        Self {
            class,
            byte_order: byte_order.into(),
            abi: abi.into(),
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
    pub fn text_region_address(self) -> u64 {
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
    /// - Places segments continuously into memory, starting from address
    ///     `machine.text_region_address() + (T::ELF_HEADER_SIZE + T::SEGMENT_HEADER_SIZE * segments.len() + T::SECTION_HEADER_SIZE * sections.len()) % 0x1000`
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
        machine: impl Into<Machine>,
        has_entry_point: bool,
        segments: Vec<SegmentTemplate<T>>,
        sections: Vec<Section<T>>,
    ) -> Result<Self> {
        let machine = machine.into();
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

// * ------------------------------------- Tests ------------------------------------ * //
#[cfg(test)]
mod tests;
