use std::{fs, io::Write as IoWrite, path::Path};

pub trait MyWriter {
    fn write_str(&mut self, s: &str) -> Result<(), ()>;
}

impl MyWriter for String {
    fn write_str(&mut self, s: &str) -> Result<(), ()> {
        self.push_str(s);
        Ok(())
    }
}

impl MyWriter for std::io::Stdout {
    fn write_str(&mut self, s: &str) -> Result<(), ()> {
        match self.write_all(s.as_bytes()) {
            Ok(()) => Ok(()),
            _ => Err(()),
        }
    }
}
impl MyWriter for std::io::Stderr {
    fn write_str(&mut self, s: &str) -> Result<(), ()> {
        match self.write_all(s.as_bytes()) {
            Ok(()) => Ok(()),
            _ => Err(()),
        }
    }
}

trait Printable {
    fn fmt_to(&self, writer: &mut dyn MyWriter) -> Result<(), ()>;
}

impl Printable for () {
    fn fmt_to(self: &Self, _: &mut dyn MyWriter) -> Result<(), ()> {
        Ok(())
    }
}

impl Printable for &str {
    fn fmt_to(&self, writer: &mut dyn MyWriter) -> Result<(), ()> {
        writer.write_str(self)?;
        writer.write_str("\n")?;
        Ok(())
    }
}

impl Printable for String {
    fn fmt_to(&self, writer: &mut dyn MyWriter) -> Result<(), ()> {
        writer.write_str(self)?;
        writer.write_str("\n")?;
        Ok(())
    }
}

// Implement Printable for integers
macro_rules! impl_printable_int {
    ($($t:ty),*) => {
        $(
            impl Printable for $t {
                fn fmt_to(&self, writer: &mut dyn MyWriter) -> Result<(),()> {
                    writer.write_str(&self.to_string())?;
                    writer.write_str("\n")?;
                    Ok(())
                }
            }
        )*
    }
}

impl_printable_int!(i8, i16, i32, i64, i128, isize, u8, u16, u32, u64, u128, usize);

pub trait PrintablePair {
    fn print_to(&self, writer: &mut dyn MyWriter) -> Result<(), ()>;
}

impl<A, B> PrintablePair for (A, B)
where
    A: Printable,
    B: Printable,
{
    fn print_to(&self, writer: &mut dyn MyWriter) -> Result<(), ()> {
        let (a, b) = self;
        a.fmt_to(writer)?;
        b.fmt_to(writer)?;
        Ok(())
    }
}

pub fn read_entire_file<P: AsRef<Path>>(path: P) -> Result<String, ()> {
    match fs::read_to_string(path) {
        Ok(string) => Ok(string),
        _ => Err(()),
    }
}
