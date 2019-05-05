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
//!
//! ## Implementation Notes
//!
//! Unfortunately the `OpenAir` format is really underspecified. Every device
//! uses varying conventions. For example, there is nothing we can use as clear
//! delimiter for airspaces. Some files delimit airspaces with an empty line,
//! some with a comment. But on the other hand, some files even place comments
//! between the coordinates so that they cannot be used as delimiter either.
//!
//! This parser tries to be very lenient when parsing, based on real life data.
//! The end of an airspace is reached when the next one starts (with an `AC`
//! record) or when the file ends.
//!
//! Note: AT records (label placement hints) are currently ignored
#![deny(clippy::all)]
#![warn(clippy::pedantic)]
#![allow(clippy::many_single_char_names)]
#![allow(clippy::non_ascii_literal)]

use std::fmt;
use std::io::BufRead;
use std::mem;

use lazy_static::lazy_static;
use log::trace;
use regex::Regex;

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
                    (Some(ft), "ft") | (Some(ft), "FT") => Ok(Altitude::FeetAmsl(ft)),
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
        lazy_static! {
            static ref RE: Regex = Regex::new(r"(?x)
                ([0-9]{1,3}[.:][0-9]{1,3}[.:][0-9]{1,3})  # Lat
                \s*
                ([NS])                                    # North / South
                \s*
                ([0-9]{1,3}[.:][0-9]{1,3}[.:][0-9]{1,3})  # Lon
                \s*
                ([EW])                                    # East / West
            ").unwrap();
        }
        let invalid = |_| format!("Invalid coord: {}", data);
        let cap = RE.captures(data).ok_or_else(|| format!("Invalid coord: {}", data))?;
        let lat = Self::multiplier_lat(&cap[2]).map_err(invalid)?
                * Self::parse_component(&cap[1]).map_err(invalid)?;
        let lng = Self::multiplier_lng(&cap[4]).map_err(invalid)?
                * Self::parse_component(&cap[3]).map_err(invalid)?;
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

/// Arc direction, either clockwise or counterclockwise.
#[derive(Debug, PartialEq, Eq)]
pub enum Direction {
    CW,
    CCW,
}

impl Direction {
    fn parse(data: &str) -> Result<Self, String> {
        match data {
            "+" => Ok(Direction::CW),
            "-" => Ok(Direction::CCW),
            _ => return Err(format!("Invalid direction: {}", data)),
        }
    }
}

/// An incomplete airspace.
#[derive(Debug)]
struct AirspaceBuilder {
    new: bool,
    name: Option<String>,
    class: Option<Class>,
    lower_bound: Option<Altitude>,
    upper_bound: Option<Altitude>,
    geom: Option<Geometry>,
    var_x: Option<Coord>,
    var_d: Option<Direction>,
}

macro_rules! setter {
    ($method:ident, $field:ident, $type:ty) => {
        fn $method(&mut self, $field: $type) -> Result<(), String> {
            self.new = false;
            if self.$field.is_some() {
                Err(format!("Could not set {} (already defined)", stringify!($field)))
            } else {
                self.$field = Some($field);
                Ok(())
            }
        }
    }
}

impl AirspaceBuilder {
    fn new() -> Self {
        Self {
            new: true,
            name: None,
            class: None,
            lower_bound: None,
            upper_bound: None,
            geom: None,
            var_x: None,
            var_d: None,
        }
    }

    setter!(set_name, name, String);
    setter!(set_class, class, Class);
    setter!(set_lower_bound, lower_bound, Altitude);
    setter!(set_upper_bound, upper_bound, Altitude);
    setter!(set_var_x, var_x, Coord);
    setter!(set_var_d, var_d, Direction);

    fn add_point(&mut self, point: Coord) -> Result<(), String> {
        self.new = false;
        match &mut self.geom {
            None => {
                self.geom = Some(Geometry::Polygon {
                    points: vec![point],
                })
            }
            Some(Geometry::Polygon { ref mut points }) => {
                points.push(point);
            }
            Some(Geometry::Circle { .. }) => {
                return Err("Cannot add a point to a circle".into());
            }
        }
        Ok(())
    }

    fn set_circle_radius(&mut self, radius: f32) -> Result<(), String> {
        self.new = false;
        let var_x = mem::replace(&mut self.var_x, None);
        match (&self.geom, var_x) {
            (None, Some(centerpoint)) => {
                self.geom = Some(Geometry::Circle { centerpoint, radius });
                Ok(())
            }
            (Some(_), _) => {
                Err("Geometry already set".into())
            }
            (_, None) => {
                Err("Centerpoint missing".into())
            }
        }
    }

    fn finish(self) -> Result<Airspace, String> {
        trace!("Finish {:?}", self.name);
        let name = self.name.ok_or("Missing name")?;
        let class = self.class.ok_or_else(|| format!("Missing class for '{}'", name))?;
        let lower_bound = self.lower_bound.ok_or_else(|| format!("Missing lower bound for '{}'", name))?;
        let upper_bound = self.upper_bound.ok_or_else(|| format!("Missing upper bound for '{}'", name))?;
        let geom = self.geom.ok_or_else(|| format!("Missing geom for '{}'", name))?;
        Ok(Airspace { name, class, lower_bound, upper_bound, geom })
    }
}

/// Return whether this line contains the start of a new airspace.
#[inline]
fn starts_airspace(line: &str) -> bool {
    line.starts_with("AC ")
}

/// Process a line.
fn process(builder: &mut AirspaceBuilder, line: &str) -> Result<(), String> {
    if line.trim().is_empty() {
        trace!("Empty line, ignoring");
        return Ok(())
    }

    let mut chars = line.chars().filter(|c: &char| !c.is_ascii_whitespace());
    let t1 = chars.next().ok_or_else(|| "Line too short".to_string())?;
    let t2 = chars.next().unwrap_or(' ');
    let data = line.splitn(2, ' ').nth(1).unwrap_or("").trim();

    trace!("Input: \"{:1}{:1}\"", t1, t2);
    match (t1, t2) {
        ('*', _) => {
            trace!("-> Comment, ignore");
        }
        ('A', 'C') => {
            // Airspace class
            let class = Class::parse(data)?;
            trace!("-> Found class: {}", class);
            builder.set_class(class)?;
        }
        ('A', 'N') => {
            trace!("-> Found name: {}", data);
            builder.set_name(data.to_string())?;
        }
        ('A', 'L') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found lower bound: {}", altitude);
            builder.set_lower_bound(altitude)?;
        }
        ('A', 'H') => {
            let altitude = Altitude::parse(data)?;
            trace!("-> Found upper bound: {}", altitude);
            builder.set_upper_bound(altitude)?;
        }
        ('A', 'T') => {
            trace!("-> Label placement hint, ignore");
        }
        ('V', 'X') => {
            trace!("-> Found X variable");
            let coord = Coord::parse(data.get(2..).unwrap_or(""))?;
            builder.set_var_x(coord)?;
        }
        ('V', 'D') => {
            trace!("-> Found D variable");
            let direction = Direction::parse(data.get(2..).unwrap_or(""))?;
            builder.set_var_d(direction)?;
        }
        ('D', 'P') => {
            trace!("-> Found point");
            let coord = Coord::parse(data)?;
            builder.add_point(coord)?;
        }
        ('D', 'C') => {
            trace!("-> Found circle radius");
            let radius = data.parse::<f32>().map_err(|_| format!("Invalid radius: {}", data))?;
            builder.set_circle_radius(radius)?;
        }
        (t1, t2) => {
            return Err(format!("Parse error (unexpected \"{:1}{:1}\")", t1, t2))
        }
    }
    Ok(())
}

/// Process the reader until EOF, return a list of found airspaces.
pub fn parse<R: BufRead>(reader: &mut R) -> Result<Vec<Airspace>, String> {
    let mut airspaces = vec![];

    let mut builder = AirspaceBuilder::new();
    loop {
        // Read next line
        let mut line = String::new();
        let bytes_read = reader.read_line(&mut line)
            .map_err(|e| format!("Could not read line: {}", e))?;
        if bytes_read == 0 {
            // EOF
            trace!("Reached EOF");
            airspaces.push(builder.finish()?);
            return Ok(airspaces);
        }

        // Trim BOM
        let trimmed_line = line.trim_start_matches('\u{feff}');

        // Determine whether we reached the start of a new airspace
        let start_of_airspace = starts_airspace(trimmed_line);

        // A new airspace starts, collect the old one first
        if start_of_airspace && !builder.new {
            let old_builder = mem::replace(&mut builder, AirspaceBuilder::new());
            airspaces.push(old_builder.finish()?);
        }

        // Process current line
        process(&mut builder, trimmed_line)?;
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
                Coord::parse("46:51:44N 009:19:42E"),
                Ok(Coord { lat: 46.86222222222222, lng: 9.328333333333333 })
            );
            assert_eq!(
                Coord::parse("46:51.44 N 009:19.42 E"),
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
            let mut spaces = parse(&mut airspace).unwrap();
            assert_eq!(spaces.len(), 1);
            let space: Airspace = spaces.pop().unwrap();
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
            let space1 = parse(&mut a1).unwrap().pop().unwrap();
            let space2 = parse(&mut a2).unwrap().pop().unwrap();
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
