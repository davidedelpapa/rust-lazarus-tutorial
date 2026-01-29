use base64::{Engine, engine::general_purpose};
use rmp_serde::{from_slice, to_vec};
use serde_json::Value;
use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};
use std::ptr;

/// FFI wrapper for MessagePack.
#[repr(C)]
pub struct MpBuffer {
    pub data: *mut u8,
    pub len: c_int,
}
impl MpBuffer {
    pub fn empty() -> Self {
        MpBuffer {
            data: ptr::null_mut(),
            len: 0,
        }
    }
}

/// Free MpBuffer's memory.
#[unsafe(no_mangle)]
pub extern "C" fn free_mpbuffer(buf: MpBuffer) {
    if !buf.data.is_null() {
        unsafe {
            drop(Vec::from_raw_parts(
                buf.data,
                buf.len as usize,
                buf.len as usize,
            ));
        }
    }
}

/// Free a C-string's memory.
#[unsafe(no_mangle)]
pub extern "C" fn free_cstring(s: *mut c_char) {
    if !s.is_null() {
        unsafe {
            drop(CString::from_raw(s));
        }
    }
}

/// Convert JSON text to MessagePack binary wrapped in a MpBuffer.
///
/// On error, returns an empty buffer
#[unsafe(no_mangle)]
pub extern "C" fn json_to_msgpack(json_ptr: *const c_char) -> MpBuffer {
    if json_ptr.is_null() {
        return MpBuffer::empty();
    }

    unsafe {
        match CStr::from_ptr(json_ptr).to_str() {
            Ok(json_str) => {
                // Parse JSON
                match serde_json::from_str::<Value>(json_str) {
                    Ok(value) => {
                        // Encode to MessagePack
                        match to_vec(&value) {
                            Ok(vec) => {
                                let len = vec.len();
                                let mut vec = vec;
                                let data = vec.as_mut_ptr();

                                // Avoid dropping the Vec
                                std::mem::forget(vec);
                                let c_len = len as c_int;

                                MpBuffer { data, len: c_len }
                            }
                            Err(_) => MpBuffer::empty(),
                        }
                    }
                    Err(_) => MpBuffer::empty(),
                }
            }
            Err(_) => MpBuffer::empty(),
        }
    }
}

/// Convert MessagePack binary to JSON C-string.
///
/// On error, returns an empty pointer
#[unsafe(no_mangle)]
pub extern "C" fn msgpack_to_json(data: *const u8, len: c_int) -> *mut c_char {
    if data.is_null() || len == 0 {
        return ptr::null_mut();
    }

    unsafe {
        let slice = std::slice::from_raw_parts(data, len as usize);

        match from_slice::<Value>(slice) {
            Ok(value) => match serde_json::to_string_pretty(&value) {
                Ok(json_str) => CString::new(json_str).unwrap().into_raw(),
                Err(_) => ptr::null_mut(),
            },
            Err(_) => ptr::null_mut(),
        }
    }
}

/// Convert JSON text to MessagePack encoded in base64 (as C-string).
///
/// On error, returns an empty pointer
#[unsafe(no_mangle)]
pub extern "C" fn json_to_msgpack64(json_ptr: *const c_char) -> *mut c_char {
    if json_ptr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        match CStr::from_ptr(json_ptr).to_str() {
            Ok(json_str) => {
                // Parse JSON
                match serde_json::from_str::<Value>(json_str) {
                    Ok(value) => {
                        // Encode to MessagePack
                        match to_vec(&value) {
                            Ok(msgpack_bytes) => {
                                // Convert to Base64
                                let b64 = general_purpose::STANDARD.encode(msgpack_bytes);
                                CString::new(b64).unwrap().into_raw()
                            }
                            Err(_) => ptr::null_mut(),
                        }
                    }
                    Err(_) => ptr::null_mut(),
                }
            }
            Err(_) => ptr::null_mut(),
        }
    }
}
/// Convert MessagePack encoded in base64 to JSON C-string.
///
/// On error, returns an empty pointer
#[unsafe(no_mangle)]
pub extern "C" fn msgpack64_to_json(b64_ptr: *const c_char) -> *mut c_char {
    if b64_ptr.is_null() {
        return ptr::null_mut();
    }

    unsafe {
        match CStr::from_ptr(b64_ptr).to_str() {
            Ok(b64_str) => {
                // Decode Base64
                match general_purpose::STANDARD.decode(b64_str) {
                    Ok(bytes) => {
                        // Decode MessagePack
                        match from_slice::<Value>(&bytes) {
                            Ok(value) => match serde_json::to_string_pretty(&value) {
                                Ok(json_str) => CString::new(json_str).unwrap().into_raw(),
                                Err(_) => ptr::null_mut(),
                            },
                            Err(_) => ptr::null_mut(),
                        }
                    }
                    Err(_) => ptr::null_mut(),
                }
            }
            Err(_) => ptr::null_mut(),
        }
    }
}

// Crate's tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trip() {
        let json = r#"{"name":"Alice","age":30}"#;
        let cjson = CString::new(json).unwrap();
        let buf = json_to_msgpack(cjson.as_ptr());

        assert!(buf.len > 0);
        assert!(!buf.data.is_null());

        let json_back = unsafe {
            let ptr = msgpack_to_json(buf.data, buf.len);
            let rust_str = CStr::from_ptr(ptr).to_str().unwrap().to_string();
            free_cstring(ptr);
            rust_str
        };

        free_mpbuffer(buf);

        assert!(json_back.contains("Alice"));
        assert!(json_back.contains("30"));
    }

    #[test]
    fn base64_roundtrip() {
        let original_json = r#"{"hello":"world","n":42}"#;

        let cjson = CString::new(original_json).unwrap();

        let b64_ptr = json_to_msgpack64(cjson.as_ptr());
        assert!(!b64_ptr.is_null());

        let json_back_ptr = msgpack64_to_json(b64_ptr);
        assert!(!json_back_ptr.is_null());

        let json_back = unsafe { CStr::from_ptr(json_back_ptr).to_str().unwrap().to_string() };

        free_cstring(b64_ptr);
        free_cstring(json_back_ptr);

        assert!(json_back.contains("hello"));
        assert!(json_back.contains("42"));
    }
}
