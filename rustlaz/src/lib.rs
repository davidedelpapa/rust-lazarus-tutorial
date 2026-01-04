use std::ffi::{CStr, CString};
use std::os::raw::{c_char, c_int};

#[unsafe(no_mangle)]
pub extern "C" fn add_numbers(a: i32, b: i32) -> i32 {
    a + b
}

#[repr(C)]
#[derive(Debug, PartialEq)]
pub struct Point {
    pub x: c_int,
    pub y: c_int,
}

#[unsafe(no_mangle)]
pub extern "C" fn move_point(p: Point, dx: c_int, dy: c_int) -> Point {
    Point {
        x: p.x + dx,
        y: p.y + dy,
    }
}

#[repr(C)]
pub struct Person {
    name: *mut c_char,
    office: *mut c_char,
    phone: *mut c_char,
    age: c_int,
}

/// Utility to convert &str to *mut c_char
fn make_cstring(s: &str) -> *mut c_char {
    CString::new(s).unwrap().into_raw()
}

/// Utility to convert *mut c_char to Rust String
unsafe fn to_rust_string(ptr: *mut c_char) -> String {
    if ptr.is_null() {
        String::new()
    } else {
        unsafe { CStr::from_ptr(ptr).to_string_lossy().to_string() }
    }
}

/// Free CStrings
#[unsafe(no_mangle)]
pub extern "C" fn free_cstring(s: *mut c_char) {
    if s.is_null() {
        return;
    }
    unsafe {
        drop(CString::from_raw(s));
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn verify_person(p: Person) -> Person {
    unsafe {
        let name = to_rust_string(p.name);
        let office = to_rust_string(p.office);
        let phone = to_rust_string(p.phone);

        // Add "(verified)" to the name
        let new_name = format!("{} {}", name, "(verified)");

        Person {
            name: make_cstring(&new_name),
            office: make_cstring(&office),
            phone: make_cstring(&phone),
            age: p.age,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn add_numbers_test() {
        let result = add_numbers(2, 2);
        assert_eq!(result, 4);
    }

    #[test]
    fn point_test() {
        let p = Point { x: 1, y: 1 };
        let p2 = move_point(p, 1, 2);
        assert_eq!(p2, Point { x: 2, y: 3 })
    }

    #[test]
    fn verify_person_strings_test() {
        use std::ffi::{CStr, CString};

        // Simulate Pascal allocating C strings
        let name_in = CString::new("John Doe").unwrap().into_raw();
        let office_in = CString::new("IT").unwrap().into_raw();
        let phone_in = CString::new("555-0101").unwrap().into_raw();

        let p = Person {
            name: name_in,
            office: office_in,
            phone: phone_in,
            age: 42,
        };
        let p2 = verify_person(p);

        // ----------------------------------------------------------
        // Read the returned Rust-allocated strings
        // ----------------------------------------------------------
        let name_out = unsafe { CStr::from_ptr(p2.name).to_string_lossy().to_string() };
        let office_out = unsafe { CStr::from_ptr(p2.office).to_string_lossy().to_string() };
        let phone_out = unsafe { CStr::from_ptr(p2.phone).to_string_lossy().to_string() };

        assert_eq!(name_out, "John Doe (verified)");
        assert_eq!(office_out, "IT");
        assert_eq!(phone_out, "555-0101");
        assert_eq!(p2.age, 42);

        // ----------------------------------------------------------
        // Free Rust-allocated strings
        // ----------------------------------------------------------
        free_cstring(p2.name);
        free_cstring(p2.office);
        free_cstring(p2.phone);

        // ----------------------------------------------------------
        // Free the simulated Pascal-allocated input strings
        // These must *not* be freed by Rust, so we free them in the test.
        // ----------------------------------------------------------
        unsafe {
            drop(CString::from_raw(name_in));
            drop(CString::from_raw(office_in));
            drop(CString::from_raw(phone_in));
        }
    }
}
