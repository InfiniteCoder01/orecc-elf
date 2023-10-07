use orecc_elf::*;

fn main() {
    let data = [
        0xB8, 0x01, 0x00, 0x00, 0x00, 0xBB, 0x00, 0x00, 0x00, 0x00, 0xCD, 0x80,
    ];
    let elf = ELF::<u32>::new(
        Ident::new(Class::ELF32, ByteOrder::LSB, ABI::None, 0),
        Type::Exec,
        Machine::X86,
        true,
        vec![SegmentTemplate::new(
            SegmentType::Load,
            data.to_vec(),
            data.len() as _,
            SegmentFlags::Readable as u32 | SegmentFlags::Executable as u32,
        )],
        Vec::new(),
    )
    .unwrap();
    elf.write(&mut std::fs::File::create("x86.out").unwrap()).unwrap();
}
