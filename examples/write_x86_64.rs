use orecc_elf::*;

fn main() {
    let data = [
        0x48, 0xC7, 0xC0, 0x3C, 0x00, 0x00, 0x00, 0x48, 0xC7, 0xC7, 0x2A, 0x00, 0x00, 0x00, 0x0F,
        0x05,
    ];
    let elf = ELF::<u64>::new(
        Ident::default(),
        Type::Exec,
        Machine::X86_64,
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
    elf.write(&mut std::fs::File::create("x86_64.out").unwrap()).unwrap();
}
