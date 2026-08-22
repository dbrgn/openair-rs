use openair::AirspaceType;

#[test]
fn parses_standard_airspace_types() {
    for (token, expected) in [
        ("ACCSEC", AirspaceType::RemoteCommunicationSector),
        ("ADIZ", AirspaceType::AirDefenceIdentZone),
        ("ALERT", AirspaceType::AlertArea),
        ("ASRA", AirspaceType::AerialSportingOrRecreationalActivity),
        ("ATZ", AirspaceType::AerodromeTrafficZone),
        ("AWY", AirspaceType::Airway),
        ("CTA", AirspaceType::ControlArea),
        ("CTR", AirspaceType::ControlZone),
        ("CUSTOM", AirspaceType::Custom),
        ("FIR", AirspaceType::FlightInformationRegion),
        ("FIS", AirspaceType::FlightInformationServiceSector),
        ("GSEC", AirspaceType::GlidingSector),
        ("HTZ", AirspaceType::HelicopterTrafficZone),
        ("LTA", AirspaceType::LowerTrafficArea),
        ("MATZ", AirspaceType::MilitaryAerodromeTrafficZone),
        ("MTA", AirspaceType::MilitaryTrainingArea),
        ("MTR", AirspaceType::MilitaryTrainingRoute),
        ("N", AirspaceType::NotamAffectedArea),
        ("NONE", AirspaceType::NoType),
        ("OFR", AirspaceType::OverflightRestriction),
        ("P", AirspaceType::ProhibitedArea),
        ("Q", AirspaceType::DangerArea),
        ("R", AirspaceType::RestrictedArea),
        ("RMZ", AirspaceType::RadioMandatoryZone),
        ("TFR", AirspaceType::TemporaryFlightRestriction),
        ("TIA", AirspaceType::TrafficInformationArea),
        ("TIZ", AirspaceType::TrafficInformationZone),
        ("TMA", AirspaceType::TerminalManoeuvringArea),
        ("TMZ", AirspaceType::TransponderMandatoryZone),
        ("TRA", AirspaceType::TemporaryReservedArea),
        (
            "TRAFR",
            AirspaceType::TemporaryReservedOrSegregatedAreaFeedingRoute,
        ),
        ("TRZ", AirspaceType::TransponderRecommendedZone),
        ("TSA", AirspaceType::TemporarySegregatedArea),
        ("UIR", AirspaceType::UpperFlightInformationRegion),
        ("UTA", AirspaceType::UpperTrafficArea),
        ("VFRR", AirspaceType::VisualFlightRulesRoute),
        ("VFRSEC", AirspaceType::VisualFlightRulesSector),
        ("WARNING", AirspaceType::WarningArea),
    ] {
        let airspace_type = AirspaceType::parse(token).unwrap();
        assert_eq!(airspace_type, expected);
        assert_eq!(airspace_type.as_str(), token);
    }
}

#[test]
fn preserves_unknown_airspace_type() {
    let airspace_type = AirspaceType::parse("FUTURE").unwrap();

    assert_eq!(airspace_type, AirspaceType::Unknown("FUTURE".into()));
    assert_eq!(airspace_type.as_str(), "FUTURE");
}

#[test]
fn rejects_empty_airspace_type() {
    assert_eq!(
        AirspaceType::parse(""),
        Err("Airspace type is empty".to_string())
    );
}
