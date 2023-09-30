use int_enum::IntEnum;
pub mod serde;
use serde::*;

// * ------------------------------------ Structs ----------------------------------- * //
#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Class {
    ELF32 = 1,
    #[default]
    ELF64 = 2,
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum ByteOrder {
    #[default]
    LSB = 1,
    MSB = 2,
}

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

#[repr(u16)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Type {
    Rel = 1,
    Exec = 2,
    Dyn = 3,
    Core = 4,
}

#[repr(u16)]
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord, IntEnum)]
pub enum Machine {
    M32 = 0x01,
    SPARC = 0x02,
    X86 = 0x03,
    M68k = 0x04,
    M88k = 0x05,
    IAMCU = 0x06,
    I860 = 0x07,
    MIPS = 0x08,
    S370 = 0x09,
    MipsRS3LE = 0x0A,
    PARISC = 0x0F,
    I960 = 0x13,
    PowerPC = 0x14,
    S390 = 0x16,
    SPU = 0x17,
    V800 = 0x24,
    FR20 = 0x25,
    RH32 = 0x26,
    MotorolaRCE = 0x27,
    ARM = 0x28,
    DigitalAlpha = 0x29,
    SuperH = 0x2A,
    SPARCV9 = 0x2B,
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
    MCSTElbrus = 0xAF,
    TMS320C6000 = 0x8C,
    Aarch64 = 0xB7,
    RISCV = 0xF3,
    BPF = 0xF7,
    WDC65C816 = 0x101,
}

pub trait SizeT: Sized + RW {}

// * ------------------------------------ E_IDENT ----------------------------------- * //
#[derive(Clone, Copy, Debug, Default, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ident {
    class: Class,
    byte_order: ByteOrder,
    abi: ABI,
    abi_version: u8,
}

impl Ident {
    pub fn new(class: Class, byte_order: ByteOrder, abi: ABI, abi_version: u8) -> Self {
        Self {
            class,
            byte_order,
            abi,
            abi_version,
        }
    }

    #[rustfmt::skip]
    pub fn write<W: std::io::Write>(&self, file: &mut W) -> Result<()> {
        file.write_all(&[
            0x7f, 0x45, 0x4c, 0x46,
            self.class as u8, self.byte_order as u8, 1,
            self.abi as u8, self.abi_version,
            0, 0, 0, 0, 0, 0, 0,
        ]).map_err(|err|Error::io(err, "e_ident"))
    }

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
#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ELF<T: SizeT> {
    ident: Ident,
    e_type: Type,
    machine: Machine,
    entry_point: T,
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
    pub fn new(ident: Ident, e_type: Type, machine: Machine, entry_point: T) -> Self {
        Self {
            ident,
            e_type,
            machine,
            entry_point,
        }
    }

    pub fn write<W: std::io::Write>(&self, file: &mut W) -> Result<()> {
        self.ident.write(file)?;
        rw_enum!(write self, e_type, file);
        rw_enum!(write self, machine, file);
        1_u32.write(file, self.ident.byte_order, "e_version")?;
        self.entry_point
            .write(file, self.ident.byte_order, "e_entry")?;
        Ok(())
    }

    pub fn read<R: std::io::Read>(file: &mut R) -> Result<Self> {
        let ident = Ident::read(file)?;
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
impl SizeT for u32 {}
impl SizeT for u64 {}

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

// #[cfg(test)]
// mod tests {
//     use super::*;

//     // #[test]
//     // fn it_works() {
//     //     let result = add(2, 2);
//     //     assert_eq!(result, 4);
//     // }
// }
// * ------------------------------------ Errors ------------------------------------ * //
pub type Result<T> = std::result::Result<T, Error>;

#[derive(Clone, Debug)]
pub enum Error {
    Signature(String),
    Error(String),
}

impl Error {
    pub fn io(err: std::io::Error, part: &str) -> Self {
        Self::Error(format!("failed to read/write {part} to/from a file: {err}"))
    }

    pub fn parse(err: impl ToString, part: &str) -> Self {
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
