use crate::{ByteOrder, Error, Result};

pub trait RW: Sized {
    fn read<R: std::io::Read>(file: &mut R, byte_order: ByteOrder, part: &str) -> Result<Self>;
    fn write<W: std::io::Write>(
        &self,
        file: &mut W,
        byte_order: ByteOrder,
        part: &str,
    ) -> Result<()>;
}

macro_rules! impl_rw {
    (integer $name: ident) => {
        impl RW for $name {
            fn read<R: std::io::Read>(
                file: &mut R,
                byte_order: ByteOrder,
                part: &str,
            ) -> Result<Self>
            where
                Self: Sized,
            {
                let mut buffer = [0_u8; Self::BITS as usize / 8];
                file.read_exact(&mut buffer)
                    .map_err(|err| Error::io(err, part))?;
                Ok(match byte_order {
                    ByteOrder::LSB => Self::from_le_bytes(buffer),
                    ByteOrder::MSB => Self::from_be_bytes(buffer),
                })
            }

            fn write<W: std::io::Write>(
                &self,
                file: &mut W,
                byte_order: ByteOrder,
                part: &str,
            ) -> Result<()> {
                file.write_all(&match byte_order {
                    ByteOrder::LSB => self.to_le_bytes(),
                    ByteOrder::MSB => self.to_be_bytes(),
                })
                .map_err(|err| Error::io(err, part))
            }
        }
    };
}

impl_rw!(integer i8);
impl_rw!(integer u8);
impl_rw!(integer i16);
impl_rw!(integer u16);
impl_rw!(integer i32);
impl_rw!(integer u32);
impl_rw!(integer i64);
impl_rw!(integer u64);
impl_rw!(integer i128);
impl_rw!(integer u128);
