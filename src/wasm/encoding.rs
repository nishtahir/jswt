use std::io;

pub fn encode_value<W>(w: &mut W, val: &u8) -> Result<usize, io::Error>
where
    W: ?Sized + io::Write,
{
    let mut bytes_written = 0;
    bytes_written += w.write(&[0x01u8])?;
    bytes_written += w.write(&[*val])?;
    Ok(bytes_written)
}

pub fn encode<W>(w: &mut W, val: &[u8]) -> Result<usize, io::Error>
where
    W: ?Sized + io::Write,
{
    let mut bytes_written = 0;
    bytes_written += w.write(&[val.len() as u8])?;
    bytes_written += w.write(val)?;
    Ok(bytes_written)
}

pub fn encode_string<W>(w: &mut W, val: &str) -> Result<usize, io::Error>
where
    W: ?Sized + io::Write,
{
    encode(w, val.as_bytes())
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_encode_string() {
        let mut buf: Vec<u8> = vec![];
        let bytes_written = encode_string(&mut buf, "test").unwrap();
        assert_eq!(buf, [4, 116, 101, 115, 116]);
        assert_eq!(bytes_written, 5);
    }

    #[test]
    fn test_encode_string_doesnt_overwrite_vec() {
        let mut buf: Vec<u8> = vec![0x02];
        let bytes_written = encode_string(&mut buf, "test").unwrap();
        assert_eq!(buf, [2, 4, 116, 101, 115, 116]);
        assert_eq!(bytes_written, 5);
    }
}
