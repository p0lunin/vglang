use criterion::{black_box, criterion_group, criterion_main, Criterion, BatchSize};
use fsmcreator::{compile_code, GlobalCtx, load_core};

pub fn criterion_benchmark(c: &mut Criterion) {
    let code = include_str!("min.vg");
    let mut ctx = GlobalCtx::new();
    load_core(&mut ctx);

    c.bench_function("compile_min", |b| {
        b.iter_batched(
            || ctx.clone(),
            |mut ctx| {
                compile_code(black_box(code), &mut ctx)
            },
            BatchSize::LargeInput
        )
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
