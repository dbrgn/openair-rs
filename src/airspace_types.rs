/// Functional type of an airspace from an OpenAir `AY` record.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AirspaceType {
    /// Communication sector for remote areas (`ACCSEC`).
    RemoteCommunicationSector,
    /// Air defence identification zone (`ADIZ`).
    AirDefenceIdentZone,
    /// Alert area (`ALERT`).
    AlertArea,
    /// Aerial sporting or recreational activity area (`ASRA`).
    AerialSportingOrRecreationalActivity,
    /// Aerodrome traffic zone (`ATZ`).
    AerodromeTrafficZone,
    /// Airway (`AWY`).
    Airway,
    /// Control area (`CTA`).
    ControlArea,
    /// Control zone (`CTR`).
    ControlZone,
    /// Custom or user-defined airspace (`CUSTOM`).
    Custom,
    /// Flight information region (`FIR`).
    FlightInformationRegion,
    /// Flight information service sector (`FIS`).
    FlightInformationServiceSector,
    /// Gliding sector (`GSEC`).
    GlidingSector,
    /// Helicopter traffic zone (`HTZ`).
    HelicopterTrafficZone,
    /// Lower traffic area (`LTA`).
    LowerTrafficArea,
    /// Military aerodrome traffic zone (`MATZ`).
    MilitaryAerodromeTrafficZone,
    /// Military training area (`MTA`).
    MilitaryTrainingArea,
    /// Military training route (`MTR`).
    MilitaryTrainingRoute,
    /// NOTAM-affected area (`N`).
    NotamAffectedArea,
    /// Airspace without a type (`NONE`).
    NoType,
    /// Overflight restriction (`OFR`).
    OverflightRestriction,
    /// Prohibited area (`P`).
    ProhibitedArea,
    /// Danger area (`Q`).
    DangerArea,
    /// Restricted area (`R`).
    RestrictedArea,
    /// Radio mandatory zone (`RMZ`).
    RadioMandatoryZone,
    /// Temporary flight restriction (`TFR`).
    TemporaryFlightRestriction,
    /// Traffic information area (`TIA`).
    TrafficInformationArea,
    /// Traffic information zone (`TIZ`).
    TrafficInformationZone,
    /// Terminal manoeuvring area (`TMA`).
    TerminalManoeuvringArea,
    /// Transponder mandatory zone (`TMZ`).
    TransponderMandatoryZone,
    /// Temporary reserved area (`TRA`).
    TemporaryReservedArea,
    /// Temporary reserved or segregated area feeding route (`TRAFR`).
    TemporaryReservedOrSegregatedAreaFeedingRoute,
    /// Transponder recommended zone (`TRZ`).
    TransponderRecommendedZone,
    /// Temporary segregated area (`TSA`).
    TemporarySegregatedArea,
    /// Upper flight information region (`UIR`).
    UpperFlightInformationRegion,
    /// Upper traffic area (`UTA`).
    UpperTrafficArea,
    /// Designated visual flight rules route (`VFRR`).
    VisualFlightRulesRoute,
    /// Visual flight rules sector (`VFRSEC`).
    VisualFlightRulesSector,
    /// Warning area (`WARNING`).
    WarningArea,
    /// Airspace type token not defined by the supported OpenAir specification.
    Unknown(Box<str>),
}

#[cfg(feature = "serde")]
impl serde::Serialize for AirspaceType {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl AirspaceType {
    /// Parses and preserves an OpenAir airspace type token.
    pub fn parse(data: &str) -> Result<Self, String> {
        if data.is_empty() {
            return Err("Airspace type is empty".to_string());
        }

        let airspace_type = match data {
            "ACCSEC" => Self::RemoteCommunicationSector,
            "ADIZ" => Self::AirDefenceIdentZone,
            "ALERT" => Self::AlertArea,
            "ASRA" => Self::AerialSportingOrRecreationalActivity,
            "ATZ" => Self::AerodromeTrafficZone,
            "AWY" => Self::Airway,
            "CTA" => Self::ControlArea,
            "CTR" => Self::ControlZone,
            "CUSTOM" => Self::Custom,
            "FIR" => Self::FlightInformationRegion,
            "FIS" => Self::FlightInformationServiceSector,
            "GSEC" => Self::GlidingSector,
            "HTZ" => Self::HelicopterTrafficZone,
            "LTA" => Self::LowerTrafficArea,
            "MATZ" => Self::MilitaryAerodromeTrafficZone,
            "MTA" => Self::MilitaryTrainingArea,
            "MTR" => Self::MilitaryTrainingRoute,
            "N" => Self::NotamAffectedArea,
            "NONE" => Self::NoType,
            "OFR" => Self::OverflightRestriction,
            "P" => Self::ProhibitedArea,
            "Q" => Self::DangerArea,
            "R" => Self::RestrictedArea,
            "RMZ" => Self::RadioMandatoryZone,
            "TFR" => Self::TemporaryFlightRestriction,
            "TIA" => Self::TrafficInformationArea,
            "TIZ" => Self::TrafficInformationZone,
            "TMA" => Self::TerminalManoeuvringArea,
            "TMZ" => Self::TransponderMandatoryZone,
            "TRA" => Self::TemporaryReservedArea,
            "TRAFR" => Self::TemporaryReservedOrSegregatedAreaFeedingRoute,
            "TRZ" => Self::TransponderRecommendedZone,
            "TSA" => Self::TemporarySegregatedArea,
            "UIR" => Self::UpperFlightInformationRegion,
            "UTA" => Self::UpperTrafficArea,
            "VFRR" => Self::VisualFlightRulesRoute,
            "VFRSEC" => Self::VisualFlightRulesSector,
            "WARNING" => Self::WarningArea,
            other => Self::Unknown(other.into()),
        };
        Ok(airspace_type)
    }

    /// Returns the original OpenAir airspace type token.
    pub fn as_str(&self) -> &str {
        match self {
            Self::RemoteCommunicationSector => "ACCSEC",
            Self::AirDefenceIdentZone => "ADIZ",
            Self::AlertArea => "ALERT",
            Self::AerialSportingOrRecreationalActivity => "ASRA",
            Self::AerodromeTrafficZone => "ATZ",
            Self::Airway => "AWY",
            Self::ControlArea => "CTA",
            Self::ControlZone => "CTR",
            Self::Custom => "CUSTOM",
            Self::FlightInformationRegion => "FIR",
            Self::FlightInformationServiceSector => "FIS",
            Self::GlidingSector => "GSEC",
            Self::HelicopterTrafficZone => "HTZ",
            Self::LowerTrafficArea => "LTA",
            Self::MilitaryAerodromeTrafficZone => "MATZ",
            Self::MilitaryTrainingArea => "MTA",
            Self::MilitaryTrainingRoute => "MTR",
            Self::NotamAffectedArea => "N",
            Self::NoType => "NONE",
            Self::OverflightRestriction => "OFR",
            Self::ProhibitedArea => "P",
            Self::DangerArea => "Q",
            Self::RestrictedArea => "R",
            Self::RadioMandatoryZone => "RMZ",
            Self::TemporaryFlightRestriction => "TFR",
            Self::TrafficInformationArea => "TIA",
            Self::TrafficInformationZone => "TIZ",
            Self::TerminalManoeuvringArea => "TMA",
            Self::TransponderMandatoryZone => "TMZ",
            Self::TemporaryReservedArea => "TRA",
            Self::TemporaryReservedOrSegregatedAreaFeedingRoute => "TRAFR",
            Self::TransponderRecommendedZone => "TRZ",
            Self::TemporarySegregatedArea => "TSA",
            Self::UpperFlightInformationRegion => "UIR",
            Self::UpperTrafficArea => "UTA",
            Self::VisualFlightRulesRoute => "VFRR",
            Self::VisualFlightRulesSector => "VFRSEC",
            Self::WarningArea => "WARNING",
            Self::Unknown(value) => value,
        }
    }
}
