use std::fmt;

/// Airspace class.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Class {
    /// Airspace A
    A,
    /// Airspace B
    B,
    /// Airspace C
    C,
    /// Airspace D
    D,
    /// Airspace E
    E,
    /// Airspace F
    F,
    /// Airspace G
    G,
    /// Unclassified
    Unclassified,
    /// Airspace class token not defined by the supported OpenAir specification.
    Unknown(Box<str>),
}

#[cfg(feature = "serde")]
impl serde::Serialize for Class {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Class {
    /// Parses and preserves an OpenAir airspace class token.
    pub fn parse(data: &str) -> Result<Self, String> {
        if data.is_empty() {
            return Err("Airspace class is empty".to_string());
        }

        match data {
            "A" => Ok(Self::A),
            "B" => Ok(Self::B),
            "C" => Ok(Self::C),
            "D" => Ok(Self::D),
            "E" => Ok(Self::E),
            "F" => Ok(Self::F),
            "G" => Ok(Self::G),
            "UNC" => Ok(Self::Unclassified),
            other => Ok(Self::Unknown(other.into())),
        }
    }

    /// Returns the original OpenAir airspace class token.
    pub fn as_str(&self) -> &str {
        match self {
            Self::A => "A",
            Self::B => "B",
            Self::C => "C",
            Self::D => "D",
            Self::E => "E",
            Self::F => "F",
            Self::G => "G",
            Self::Unclassified => "UNC",
            Self::Unknown(value) => value,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_standard_classes() {
        for (token, expected) in [
            ("A", Class::A),
            ("B", Class::B),
            ("C", Class::C),
            ("D", Class::D),
            ("E", Class::E),
            ("F", Class::F),
            ("G", Class::G),
            ("UNC", Class::Unclassified),
        ] {
            let class = Class::parse(token).unwrap();
            assert_eq!(class, expected);
            assert_eq!(class.as_str(), token);
        }
    }

    #[test]
    fn preserves_unknown_class() {
        let class = Class::parse("R").unwrap();

        assert_eq!(class, Class::Unknown("R".into()));
        assert_eq!(class.as_str(), "R");
    }

    #[test]
    fn rejects_empty_class() {
        assert_eq!(Class::parse(""), Err("Airspace class is empty".to_string()));
    }
}
