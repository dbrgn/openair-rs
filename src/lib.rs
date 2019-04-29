//! Simple line-based parser for airspace files in `OpenAir` format (used by
//! flight instruments like Skytraxx and others).
//!
//! <http://www.winpilot.com/UsersGuide/UserAirspace.asp>
//!
//! If you want to use this library, you need the [`parse`](fn.parse.html)
//! function as entry point.
//!
//! For an example on how to use the parse function, see the examples in the
//! source repository.
#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::non_ascii_literal)]

use std::fmt;
use std::io::BufRead;

use log::trace;

#[cfg(feature = "serde")]
use serde::Serialize;

/// Airspace class.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
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
    /// Controlled Traffic Region
    CTR,
    /// Restricted area
    Restricted,
    /// Danger area
    Danger,
    /// Prohibited area
    Prohibited,
    /// Prohibited for gliders
    GliderProhibited,
    /// Wave window
    WaveWindow,
}

impl fmt::Display for Class {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Class {
    fn parse(data: &str) -> Result<Self, String> {
        match data {
            "A" => Ok(Class::A),
            "B" => Ok(Class::B),
            "C" => Ok(Class::C),
            "D" => Ok(Class::D),
            "E" => Ok(Class::E),
            "CTR" => Ok(Class::CTR),
            "R" => Ok(Class::Restricted),
            "Q" => Ok(Class::Danger),
            "P" => Ok(Class::Prohibited),
            "GP" => Ok(Class::GliderProhibited),
            "W" => Ok(Class::WaveWindow),
            other => Err(format!("Invalid class: {}", other))
        }
    }
}

/// Altitude, either ground or a certain height AMSL in feet.
#[derive(Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type", content = "val"))]
pub enum Altitude {
    /// Ground/surface level
    Gnd,
    /// Feet above mean sea level
    FeetAmsl(i32),
    /// Feet above ground level
    FeetAgl(i32),
}

impl fmt::Display for Altitude {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Altitude::Gnd => write!(f, "GND"),
            Altitude::FeetAmsl(ft) => write!(f, "{} ft AMSL", ft),
            Altitude::FeetAgl(ft) => write!(f, "{} ft AGL", ft),
        }
    }
}

impl Altitude {
    fn parse(data: &str) -> Result<Self, String> {
        match data {
            "gnd" | "Gnd" | "GND" | "sfc" | "Sfc" | "SFC" => {
                // Note: SFC = Surface. Seems to be another abbreviation for GND.
                Ok(Altitude::Gnd)
            }
            other => {
                let is_digit = |c: &char| c.is_digit(10);
                let number: String = other.chars().take_while(is_digit).collect();
                let rest: String = other.chars().skip_while(is_digit).collect();
                match (number.parse::<i32>().ok(), rest.trim()) {
                    (Some(ft), "ft") => Ok(Altitude::FeetAmsl(ft)),
                    (Some(ft), "FT") => Ok(Altitude::FeetAmsl(ft)),
                    (Some(ft), "ft AGL") => Ok(Altitude::FeetAgl(ft)),
                    _ => Err(format!("Invalid altitude: {:?}", other))
                }
            }
        }
    }
}

/// A coordinate pair (WGS84).
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
pub struct Coord {
    lat: f64,
    lng: f64,
}

impl Coord {
    fn parse_number_opt(val: Option<&str>) -> Result<u16, ()> {
        val.and_then(|v| v.parse::<u16>().ok()).ok_or(())
    }

    fn parse_component(val: &str) -> Result<f64, ()> {
        let mut parts = val.split(|c| c == ':' || c == '.');
        let deg = Self::parse_number_opt(parts.next())?;
        let min = Self::parse_number_opt(parts.next())?;
        let sec = Self::parse_number_opt(parts.next())?;
        Ok(f64::from(deg) + f64::from(min) / 60.0 + f64::from(sec) / 3600.0)
    }

    fn multiplier_lat(val: &str) -> Result<f64, ()> {
        match val {
            "N" => Ok(1.0),
            "S" => Ok(-1.0),
            _ => Err(())
        }
    }

    fn multiplier_lng(val: &str) -> Result<f64, ()> {
        match val {
            "E" => Ok(1.0),
            "W" => Ok(-1.0),
            _ => Err(())
        }
    }

    fn parse(data: &str) -> Result<Self, String> {
        let parts: Vec<&str> = data.split(' ').collect();
        let invalid = |_| format!("Invalid coord: {}", data);
        if parts.len() != 4 {
            return Err(invalid(()));
        }
        let lat = Self::multiplier_lat(parts[1]).map_err(invalid)?
                * Self::parse_component(parts[0]).map_err(invalid)?;
        let lng = Self::multiplier_lng(parts[3]).map_err(invalid)?
                * Self::parse_component(parts[2]).map_err(invalid)?;
        Ok(Self { lat, lng })
    }
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum Geometry {
    Polygon {
        /// Points describing the polygon.
        ///
        /// The polygon may be open or closed.
        points: Vec<Coord>
    },
    Circle {
        /// The centerpoint of the circle.
        centerpoint: Coord,
        /// Radius of the circle in nautical miles (1 NM = 1852 m).
        radius: f32,
    },
}

impl fmt::Display for Geometry {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Geometry::Polygon { points } => write!(f, "Polygon[{}]", points.len()),
            Geometry::Circle { radius, .. } => write!(f, "Circle[r={}NM]", radius),
        }
    }
}

/// An airspace.
#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "camelCase"))]
pub struct Airspace {
    /// The name / description of the airspace
    pub name: String,
    /// The airspace class
    pub class: Class,
    /// The lower bound of the airspace
    pub lower_bound: Altitude,
    /// The upper bound of the airspace
    pub upper_bound: Altitude,
    /// The airspace geometry
    pub geom: Geometry,
}

impl fmt::Display for Airspace {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} [{}] ({} → {}) {{{}}}",
            self.name,
            self.class,
            self.lower_bound,
            self.upper_bound,
            self.geom,
        )
    }
}

#[derive(Debug, PartialEq, Eq)]
enum Bound {
    Upper,
    Lower,
}

#[derive(Debug)]
enum ParsingState {
    New,
    HasClass(Class),
    HasName(Class, String),
    HasOneBound(Class, String, Bound, Altitude),
    HasBothBounds(Class, String, Altitude, Altitude),
    ParsingPolygon(Class, String, Altitude, Altitude, Vec<Coord>),
    ParsingCircle(Class, String, Altitude, Altitude, Coord),
    Done(Airspace),
}

impl fmt::Display for ParsingState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", match self {
            ParsingState::New => "New",
            ParsingState::HasClass(..) => "HasClass",
            ParsingState::HasName(..) => "HasName",
            ParsingState::HasOneBound(..) => "HasOneBound",
            ParsingState::HasBothBounds(..) => "HasBothBounds",
            ParsingState::ParsingPolygon(..) => "ParsingPolygon",
            ParsingState::ParsingCircle(..) => "ParsingCircle",
            ParsingState::Done(..) => "Done",
        })
    }
}

/// Process a line based on the current state. Return a new state or an error.
fn process(state: ParsingState, line: &str) -> Result<ParsingState, String> {
    if line.trim().is_empty() {
        trace!("Empty line, ignoring");
        return Ok(state)
    }

    let mut chars = line.chars();
    let t1 = chars.next().ok_or_else(|| "Line too short".to_string())?;
    let t2 = chars.next().ok_or_else(|| "Line too short".to_string())?;
    let data = line.get(3..).unwrap_or("").trim();

    trace!("State: \"{}\", Input: \"{:1}{:1}\"", state, t1, t2);
    match (state, t1, t2) {
        (ParsingState::New, '*', _) => {
            // Comment, ignore
            trace!("-> Ignore");
            Ok(ParsingState::New)
        }
        (ParsingState::New, 'A', 'C') => {
            // Airspace class
            let class = Class::parse(data)?;
            trace!("-> Found class: {}", class);
            Ok(ParsingState::HasClass(class))
        }

        (ParsingState::HasClass(c), 'A', 'N') => {
            trace!("-> Found name: {}", data);
            Ok(ParsingState::HasName(c, data.to_string()))
        }

        (ParsingState::HasName(c, n), 'A', 'L') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found lower bound: {}", altitude);
            Ok(ParsingState::HasOneBound(c, n, Bound::Lower, altitude))
        }
        (ParsingState::HasName(c, n), 'A', 'H') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found upper bound: {}", altitude);
            Ok(ParsingState::HasOneBound(c, n, Bound::Upper, altitude))
        }

        (ParsingState::HasOneBound(c, n, Bound::Upper, a), 'A', 'L') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found lower bound: {}", altitude);
            Ok(ParsingState::HasBothBounds(c, n, altitude, a))
        }
        (ParsingState::HasOneBound(c, n, Bound::Lower, a), 'A', 'H') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found upper bound: {}", altitude);
            Ok(ParsingState::HasBothBounds(c, n, a, altitude))
        }

        (ParsingState::HasBothBounds(c, n, l, u), 'D', 'P') => {
            trace!("-> Found point");
            let coords = vec![Coord::parse(data)?];
            Ok(ParsingState::ParsingPolygon(c, n, l, u, coords))
        }
        (ParsingState::HasBothBounds(c, n, l, u), 'V', _) => {
            trace!("-> Found centerpoint");
            let centerpoint = Coord::parse(data.get(1..).unwrap_or(""))?;
            Ok(ParsingState::ParsingCircle(c, n, l, u, centerpoint))
        }

        (ParsingState::ParsingPolygon(c, n, l, u, mut p), 'D', 'P') => {
            trace!("-> Found point");
            p.push(Coord::parse(data)?);
            Ok(ParsingState::ParsingPolygon(c, n, l, u, p))
        }
        (ParsingState::ParsingPolygon(c, n, l, u, p), '*', _) => {
            trace!("-> Done parsing polygon");
            if p.len() < 2 {
                return Err(format!("Invalid airspace polygon (only {} points)", p.len()));
            }
            Ok(ParsingState::Done(Airspace {
                name: n,
                class: c,
                lower_bound: l,
                upper_bound: u,
                geom: Geometry::Polygon { points: p },
            }))
        }

        (ParsingState::ParsingCircle(c, n, l, u, p), 'D', 'C') => {
            trace!("-> Found point");
            let radius = data.parse::<f32>().map_err(|_| format!("Invalid radius: {}", data))?;
            Ok(ParsingState::Done(Airspace {
                name: n,
                class: c,
                lower_bound: l,
                upper_bound: u,
                geom: Geometry::Circle { centerpoint: p, radius },
            }))
        }

        (state, t1, t2) => {
            Err(format!("Parse error in state \"{}\" (unexpected \"{:1}{:1}\")", state, t1, t2))
        }
    }
}

/// Process the reader line by line and return the next complete airspace.
///
/// When the end of the reader has been reached, return `None`.
pub fn parse<R: BufRead>(reader: &mut R) -> Result<Option<Airspace>, String> {
    let mut state = ParsingState::New;
    loop {
        // Read next line
        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)
            .map_err(|e| format!("Could not read line: {}", e))?;
        if bytes_read == 0 {
            // EOF
            return Ok(None);
        }

        // Trim BOM
        let trimmed_line = line.trim_start_matches('\u{feff}');

        // Find next state
        state = process(state, trimmed_line)?;

        if let ParsingState::Done(airspace) = state {
            return Ok(Some(airspace))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use indoc::indoc;

    mod coord {
        use super::*;

        #[test]
        #[allow(clippy::unreadable_literal)]
        fn parse() {
            assert_eq!(
                Coord::parse("46:51:44 N 009:19:42 E"),
                Ok(Coord { lat: 46.86222222222222, lng: 9.328333333333333 })
            );
            assert_eq!(
                Coord::parse("46:51:44 S 009:19:42 W"),
                Ok(Coord { lat: -46.86222222222222, lng: -9.328333333333333 })
            );
            assert_eq!(
                Coord::parse("46:51:44 Q 009:19:42 R"),
                Err("Invalid coord: 46:51:44 Q 009:19:42 R".to_string())
            );
        }
    }

    mod altitude {
        use super::*;

        #[test]
        fn parse_gnd() {
            assert_eq!(Altitude::parse("gnd").unwrap(), Altitude::Gnd);
            assert_eq!(Altitude::parse("Gnd").unwrap(), Altitude::Gnd);
            assert_eq!(Altitude::parse("GND").unwrap(), Altitude::Gnd);
            assert_eq!(Altitude::parse("sfc").unwrap(), Altitude::Gnd);
            assert_eq!(Altitude::parse("Sfc").unwrap(), Altitude::Gnd);
            assert_eq!(Altitude::parse("SFC").unwrap(), Altitude::Gnd);
        }

        #[test]
        fn parse_amsl() {
            assert_eq!(Altitude::parse("42 ft").unwrap(), Altitude::FeetAmsl(42));
            assert_eq!(Altitude::parse("42 FT").unwrap(), Altitude::FeetAmsl(42));
            assert_eq!(Altitude::parse("42ft").unwrap(), Altitude::FeetAmsl(42));
            assert_eq!(Altitude::parse("42  ft").unwrap(), Altitude::FeetAmsl(42));
        }
    }

    mod parse_airspace {
        use super::*;

        /// Parse an airspace as generated by flyland.
        #[test]
        fn flyland_buochs() {
            let mut airspace = indoc!("
                AC D
                AN BUOCHS Be CTR 119.625
                AL GND
                AH 12959 ft
                DP 46:57:13 N 008:27:52 E
                DP 46:57:46 N 008:30:41 E
                DP 46:57:55 N 008:28:40 E
                DP 46:58:28 N 008:27:56 E
                DP 46:57:13 N 008:27:52 E
                * n-Points: 5
            ").as_bytes();
            let space = parse(&mut airspace).unwrap().unwrap();
            assert_eq!(space.name, "BUOCHS Be CTR 119.625");
            assert_eq!(space.lower_bound, Altitude::Gnd);
            assert_eq!(space.upper_bound, Altitude::FeetAmsl(12959));
            if let Geometry::Polygon { points } = space.geom {
                assert_eq!(points.len(), 5);
            } else {
                panic!("Unexpected enum variant");
            }
        }

        /// The order of bounds should not matter.
        #[test]
        fn inverted_bounds() {
            let mut a1 = indoc!("
                AC D
                AN SOMESPACE
                AL GND
                AH 12959 ft
                DP 46:57:13 N 008:27:52 E
                DP 46:57:46 N 008:30:41 E
                *
            ").as_bytes();
            let mut a2 = indoc!("
                AC D
                AN SOMESPACE
                AH 12959 ft
                AL GND
                DP 46:57:13 N 008:27:52 E
                DP 46:57:46 N 008:30:41 E
                *
            ").as_bytes();
            let space1 = parse(&mut a1).unwrap().unwrap();
            let space2 = parse(&mut a2).unwrap().unwrap();
            assert_eq!(space1, space2);
        }
    }

    #[cfg(feature = "serde")]
    mod serde {
        use super::*;
        use serde_json::to_string;

        #[test]
        fn serialize_json() {
            let airspace = Airspace {
                name: "SUPERSPACE".into(),
                class: Class::Prohibited,
                lower_bound: Altitude::Gnd,
                upper_bound: Altitude::FeetAgl(3000),
                geom: Geometry::Polygon {
                    points: vec![
                        Coord { lat: 1.0, lng: 2.0 },
                        Coord { lat: 1.1, lng: 2.0 },
                        Coord { lat: 1.1, lng: 2.1 },
                        Coord { lat: 1.0, lng: 2.1 },
                        Coord { lat: 1.0, lng: 2.0 },
                    ],
                },
            };
            assert_eq!(
                to_string(&airspace).unwrap(),
                "{\"name\":\"SUPERSPACE\",\
                  \"class\":\"Prohibited\",\
                  \"lowerBound\":{\"type\":\"Gnd\"},\
                  \"upperBound\":{\"type\":\"FeetAgl\",\"val\":3000},\
                  \"geom\":{\
                    \"type\":\"Polygon\",\
                    \"points\":[\
                      {\"lat\":1.0,\"lng\":2.0},\
                      {\"lat\":1.1,\"lng\":2.0},\
                      {\"lat\":1.1,\"lng\":2.1},\
                      {\"lat\":1.0,\"lng\":2.1},\
                      {\"lat\":1.0,\"lng\":2.0}\
                    ]\
                  }\
                 }"
            );
        }
    }
}
