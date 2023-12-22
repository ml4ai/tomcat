from typing import Optional

from sqlalchemy import ForeignKey
from sqlalchemy import Integer
from sqlalchemy import REAL
from sqlalchemy import Text
from sqlalchemy import func
from sqlalchemy.orm import Mapped
from sqlalchemy.orm import Session
from sqlalchemy.orm import mapped_column

from datasette_interface.database.entity.base.base import Base


class AudioVocalics(Base):
    __tablename__ = "audio_vocalics"

    group_session_id: Mapped[str] = mapped_column("group_session", Text,
                                                  ForeignKey("group_session.id"),
                                                  primary_key=True)
    station_id: Mapped[str] = mapped_column("station", Text, ForeignKey("station.id"),
                                            primary_key=True)
    minecraft_mission_id: Mapped[str] = mapped_column("minecraft_mission_id", Text,
                                                      ForeignKey("minecraft_mission.id"),
                                                      primary_key=True)
    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    timestamp_unix: Mapped[str] = mapped_column(Text)
    timestamp_iso8601: Mapped[str] = mapped_column(Text)
    frame_time: Mapped[float] = mapped_column("frame_time", REAL)
    f0final_sma: Mapped[float] = mapped_column("f0final_sma", REAL)
    voicingfinalunclipped_sma: Mapped[float] = mapped_column("voicingfinalunclipped_sma", REAL)
    jitterlocal_sma: Mapped[float] = mapped_column("jitterlocal_sma", REAL)
    jitterddp_sma: Mapped[float] = mapped_column("jitterddp_sma", REAL)
    shimmerlocal_sma: Mapped[float] = mapped_column("shimmerlocal_sma", REAL)
    loghnr_sma: Mapped[float] = mapped_column("loghnr_sma", REAL)
    audspec_lengthl1norm_sma: Mapped[float] = mapped_column("audspec_lengthl1norm_sma", REAL)
    audspecrasta_lengthl1norm_sma: Mapped[float] = mapped_column("audspecrasta_lengthl1norm_sma",
                                                                 REAL)
    wave_rmsenergy_sma: Mapped[float] = mapped_column("wave_rmsenergy_sma", REAL)
    wave_zcr_sma: Mapped[float] = mapped_column("wave_zcr_sma", REAL)
    audspec_rfilt_sma0: Mapped[float] = mapped_column("audspec_rfilt_sma[0]", REAL)
    audspec_rfilt_sma1: Mapped[float] = mapped_column("audspec_rfilt_sma[1]", REAL)
    audspec_rfilt_sma2: Mapped[float] = mapped_column("audspec_rfilt_sma[2]", REAL)
    audspec_rfilt_sma3: Mapped[float] = mapped_column("audspec_rfilt_sma[3]", REAL)
    audspec_rfilt_sma4: Mapped[float] = mapped_column("audspec_rfilt_sma[4]", REAL)
    audspec_rfilt_sma5: Mapped[float] = mapped_column("audspec_rfilt_sma[5]", REAL)
    audspec_rfilt_sma6: Mapped[float] = mapped_column("audspec_rfilt_sma[6]", REAL)
    audspec_rfilt_sma7: Mapped[float] = mapped_column("audspec_rfilt_sma[7]", REAL)
    audspec_rfilt_sma8: Mapped[float] = mapped_column("audspec_rfilt_sma[8]", REAL)
    audspec_rfilt_sma9: Mapped[float] = mapped_column("audspec_rfilt_sma[9]", REAL)
    audspec_rfilt_sma10: Mapped[float] = mapped_column("audspec_rfilt_sma[10]", REAL)
    audspec_rfilt_sma11: Mapped[float] = mapped_column("audspec_rfilt_sma[11]", REAL)
    audspec_rfilt_sma12: Mapped[float] = mapped_column("audspec_rfilt_sma[12]", REAL)
    audspec_rfilt_sma13: Mapped[float] = mapped_column("audspec_rfilt_sma[13]", REAL)
    audspec_rfilt_sma14: Mapped[float] = mapped_column("audspec_rfilt_sma[14]", REAL)
    audspec_rfilt_sma15: Mapped[float] = mapped_column("audspec_rfilt_sma[15]", REAL)
    audspec_rfilt_sma16: Mapped[float] = mapped_column("audspec_rfilt_sma[16]", REAL)
    audspec_rfilt_sma17: Mapped[float] = mapped_column("audspec_rfilt_sma[17]", REAL)
    audspec_rfilt_sma18: Mapped[float] = mapped_column("audspec_rfilt_sma[18]", REAL)
    audspec_rfilt_sma19: Mapped[float] = mapped_column("audspec_rfilt_sma[19]", REAL)
    audspec_rfilt_sma20: Mapped[float] = mapped_column("audspec_rfilt_sma[20]", REAL)
    audspec_rfilt_sma21: Mapped[float] = mapped_column("audspec_rfilt_sma[21]", REAL)
    audspec_rfilt_sma22: Mapped[float] = mapped_column("audspec_rfilt_sma[22]", REAL)
    audspec_rfilt_sma23: Mapped[float] = mapped_column("audspec_rfilt_sma[23]", REAL)
    audspec_rfilt_sma24: Mapped[float] = mapped_column("audspec_rfilt_sma[24]", REAL)
    audspec_rfilt_sma25: Mapped[float] = mapped_column("audspec_rfilt_sma[25]", REAL)
    wave_fftmag_fband250_650_sma: Mapped[float] = mapped_column("wave_fftmag_fband250-650_sma",
                                                                REAL)
    wave_fftmag_fband1000_4000_sma: Mapped[float] = mapped_column("wave_fftmag_fband1000-4000_sma",
                                                                  REAL)
    wave_fftmag_spectralrolloff25_0_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff25.0_sma", REAL)
    wave_fftmag_spectralrolloff50_0_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff50.0_sma", REAL)
    wave_fftmag_spectralrolloff75_0_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff75.0_sma", REAL)
    wave_fftmag_spectralrolloff90_0_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff90.0_sma", REAL)
    wave_fftmag_spectralflux_sma: Mapped[float] = mapped_column("wave_fftmag_spectralflux_sma",
                                                                REAL)
    wave_fftmag_spectralcentroid_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralcentroid_sma", REAL)
    wave_fftmag_spectralentropy_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralentropy_sma", REAL)
    wave_fftmag_spectralvariance_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralvariance_sma", REAL)
    wave_fftmag_spectralskewness_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralskewness_sma", REAL)
    wave_fftmag_spectralkurtosis_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralkurtosis_sma", REAL)
    wave_fftmag_spectralslope_sma: Mapped[float] = mapped_column("wave_fftmag_spectralslope_sma",
                                                                 REAL)
    wave_fftmag_psysharpness_sma: Mapped[float] = mapped_column("wave_fftmag_psysharpness_sma",
                                                                REAL)
    wave_fftmag_spectralharmonicity_sma: Mapped[float] = mapped_column(
        "wave_fftmag_spectralharmonicity_sma", REAL)
    mfcc_sma1: Mapped[float] = mapped_column("mfcc_sma[1]", REAL)
    mfcc_sma2: Mapped[float] = mapped_column("mfcc_sma[2]", REAL)
    mfcc_sma3: Mapped[float] = mapped_column("mfcc_sma[3]", REAL)
    mfcc_sma4: Mapped[float] = mapped_column("mfcc_sma[4]", REAL)
    mfcc_sma5: Mapped[float] = mapped_column("mfcc_sma[5]", REAL)
    mfcc_sma6: Mapped[float] = mapped_column("mfcc_sma[6]", REAL)
    mfcc_sma7: Mapped[float] = mapped_column("mfcc_sma[7]", REAL)
    mfcc_sma8: Mapped[float] = mapped_column("mfcc_sma[8]", REAL)
    mfcc_sma9: Mapped[float] = mapped_column("mfcc_sma[9]", REAL)
    mfcc_sma10: Mapped[float] = mapped_column("mfcc_sma[10]", REAL)
    mfcc_sma11: Mapped[float] = mapped_column("mfcc_sma[11]", REAL)
    mfcc_sma12: Mapped[float] = mapped_column("mfcc_sma[12]", REAL)
    mfcc_sma13: Mapped[float] = mapped_column("mfcc_sma[13]", REAL)
    mfcc_sma14: Mapped[float] = mapped_column("mfcc_sma[14]", REAL)
    f0final_sma_de: Mapped[float] = mapped_column("f0final_sma_de", REAL)
    voicingfinalunclipped_sma_de: Mapped[float] = mapped_column("voicingfinalunclipped_sma_de",
                                                                REAL)
    jitterlocal_sma_de: Mapped[float] = mapped_column("jitterlocal_sma_de", REAL)
    jitterddp_sma_de: Mapped[float] = mapped_column("jitterddp_sma_de", REAL)
    shimmerlocal_sma_de: Mapped[float] = mapped_column("shimmerlocal_sma_de", REAL)
    loghnr_sma_de: Mapped[float] = mapped_column("loghnr_sma_de", REAL)
    audspec_lengthl1norm_sma_de: Mapped[float] = mapped_column("audspec_lengthl1norm_sma_de", REAL)
    audspecrasta_lengthl1norm_sma_de: Mapped[float] = mapped_column(
        "audspecrasta_lengthl1norm_sma_de", REAL)
    wave_rmsenergy_sma_de: Mapped[float] = mapped_column("wave_rmsenergy_sma_de", REAL)
    wave_zcr_sma_de: Mapped[float] = mapped_column("wave_zcr_sma_de", REAL)
    audspec_rfilt_sma_de0: Mapped[float] = mapped_column("audspec_rfilt_sma_de[0]", REAL)
    audspec_rfilt_sma_de1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[1]", REAL)
    audspec_rfilt_sma_de2: Mapped[float] = mapped_column("audspec_rfilt_sma_de[2]", REAL)
    audspec_rfilt_sma_de3: Mapped[float] = mapped_column("audspec_rfilt_sma_de[3]", REAL)
    audspec_rfilt_sma_de4: Mapped[float] = mapped_column("audspec_rfilt_sma_de[4]", REAL)
    audspec_rfilt_sma_de5: Mapped[float] = mapped_column("audspec_rfilt_sma_de[5]", REAL)
    audspec_rfilt_sma_de6: Mapped[float] = mapped_column("audspec_rfilt_sma_de[6]", REAL)
    audspec_rfilt_sma_de7: Mapped[float] = mapped_column("audspec_rfilt_sma_de[7]", REAL)
    audspec_rfilt_sma_de8: Mapped[float] = mapped_column("audspec_rfilt_sma_de[8]", REAL)
    audspec_rfilt_sma_de9: Mapped[float] = mapped_column("audspec_rfilt_sma_de[9]", REAL)
    audspec_rfilt_sma_de10: Mapped[float] = mapped_column("audspec_rfilt_sma_de[10]", REAL)
    audspec_rfilt_sma_de11: Mapped[float] = mapped_column("audspec_rfilt_sma_de[11]", REAL)
    audspec_rfilt_sma_de12: Mapped[float] = mapped_column("audspec_rfilt_sma_de[12]", REAL)
    audspec_rfilt_sma_de13: Mapped[float] = mapped_column("audspec_rfilt_sma_de[13]", REAL)
    audspec_rfilt_sma_de14: Mapped[float] = mapped_column("audspec_rfilt_sma_de[14]", REAL)
    audspec_rfilt_sma_de15: Mapped[float] = mapped_column("audspec_rfilt_sma_de[15]", REAL)
    audspec_rfilt_sma_de16: Mapped[float] = mapped_column("audspec_rfilt_sma_de[16]", REAL)
    audspec_rfilt_sma_de17: Mapped[float] = mapped_column("audspec_rfilt_sma_de[17]", REAL)
    audspec_rfilt_sma_de18: Mapped[float] = mapped_column("audspec_rfilt_sma_de[18]", REAL)
    audspec_rfilt_sma_de19: Mapped[float] = mapped_column("audspec_rfilt_sma_de[19]", REAL)
    audspec_rfilt_sma_de20: Mapped[float] = mapped_column("audspec_rfilt_sma_de[20]", REAL)
    audspec_rfilt_sma_de21: Mapped[float] = mapped_column("audspec_rfilt_sma_de[21]", REAL)
    audspec_rfilt_sma_de22: Mapped[float] = mapped_column("audspec_rfilt_sma_de[22]", REAL)
    audspec_rfilt_sma_de23: Mapped[float] = mapped_column("audspec_rfilt_sma_de[23]", REAL)
    audspec_rfilt_sma_de24: Mapped[float] = mapped_column("audspec_rfilt_sma_de[24]", REAL)
    audspec_rfilt_sma_de25: Mapped[float] = mapped_column("audspec_rfilt_sma_de[25]", REAL)
    wave_fftmag_fband250_650_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_fband250-650_sma_de", REAL)
    wave_fftmag_fband1000_4000_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_fband1000-4000_sma_de", REAL)
    wave_fftmag_spectralrolloff25_0_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff25.0_sma_de", REAL)
    wave_fftmag_spectralrolloff50_0_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff50.0_sma_de", REAL)
    wave_fftmag_spectralrolloff75_0_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff75.0_sma_de", REAL)
    wave_fftmag_spectralrolloff90_0_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff90.0_sma_de", REAL)
    wave_fftmag_spectralflux_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralflux_sma_de", REAL)
    wave_fftmag_spectralcentroid_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralcentroid_sma_de", REAL)
    wave_fftmag_spectralentropy_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralentropy_sma_de", REAL)
    wave_fftmag_spectralvariance_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralvariance_sma_de", REAL)
    wave_fftmag_spectralskewness_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralskewness_sma_de", REAL)
    wave_fftmag_spectralkurtosis_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralkurtosis_sma_de", REAL)
    wave_fftmag_spectralslope_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralslope_sma_de", REAL)
    wave_fftmag_psysharpness_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_psysharpness_sma_de", REAL)
    wave_fftmag_spectralharmonicity_sma_de: Mapped[float] = mapped_column(
        "wave_fftmag_spectralharmonicity_sma_de", REAL)
    mfcc_sma_de1: Mapped[float] = mapped_column("mfcc_sma_de[1]", REAL)
    mfcc_sma_de2: Mapped[float] = mapped_column("mfcc_sma_de[2]", REAL)
    mfcc_sma_de3: Mapped[float] = mapped_column("mfcc_sma_de[3]", REAL)
    mfcc_sma_de4: Mapped[float] = mapped_column("mfcc_sma_de[4]", REAL)
    mfcc_sma_de5: Mapped[float] = mapped_column("mfcc_sma_de[5]", REAL)
    mfcc_sma_de6: Mapped[float] = mapped_column("mfcc_sma_de[6]", REAL)
    mfcc_sma_de7: Mapped[float] = mapped_column("mfcc_sma_de[7]", REAL)
    mfcc_sma_de8: Mapped[float] = mapped_column("mfcc_sma_de[8]", REAL)
    mfcc_sma_de9: Mapped[float] = mapped_column("mfcc_sma_de[9]", REAL)
    mfcc_sma_de10: Mapped[float] = mapped_column("mfcc_sma_de[10]", REAL)
    mfcc_sma_de11: Mapped[float] = mapped_column("mfcc_sma_de[11]", REAL)
    mfcc_sma_de12: Mapped[float] = mapped_column("mfcc_sma_de[12]", REAL)
    mfcc_sma_de13: Mapped[float] = mapped_column("mfcc_sma_de[13]", REAL)
    mfcc_sma_de14: Mapped[float] = mapped_column("mfcc_sma_de[14]", REAL)
    f0final_sma_de_1: Mapped[float] = mapped_column("f0final_sma_de.1", REAL)
    voicingfinalunclipped_sma_de_1: Mapped[float] = mapped_column("voicingfinalunclipped_sma_de.1",
                                                                  REAL)
    jitterlocal_sma_de_1: Mapped[float] = mapped_column("jitterlocal_sma_de.1", REAL)
    jitterddp_sma_de_1: Mapped[float] = mapped_column("jitterddp_sma_de.1", REAL)
    shimmerlocal_sma_de_1: Mapped[float] = mapped_column("shimmerlocal_sma_de.1", REAL)
    loghnr_sma_de_1: Mapped[float] = mapped_column("loghnr_sma_de.1", REAL)
    audspec_lengthl1norm_sma_de_1: Mapped[float] = mapped_column("audspec_lengthl1norm_sma_de.1",
                                                                 REAL)
    audspecrasta_lengthl1norm_sma_de_1: Mapped[float] = mapped_column(
        "audspecrasta_lengthl1norm_sma_de.1", REAL)
    wave_rmsenergy_sma_de_1: Mapped[float] = mapped_column("wave_rmsenergy_sma_de.1", REAL)
    wave_zcr_sma_de_1: Mapped[float] = mapped_column("wave_zcr_sma_de.1", REAL)
    audspec_rfilt_sma_de0_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[0].1", REAL)
    audspec_rfilt_sma_de1_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[1].1", REAL)
    audspec_rfilt_sma_de2_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[2].1", REAL)
    audspec_rfilt_sma_de3_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[3].1", REAL)
    audspec_rfilt_sma_de4_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[4].1", REAL)
    audspec_rfilt_sma_de5_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[5].1", REAL)
    audspec_rfilt_sma_de6_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[6].1", REAL)
    audspec_rfilt_sma_de7_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[7].1", REAL)
    audspec_rfilt_sma_de8_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[8].1", REAL)
    audspec_rfilt_sma_de9_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[9].1", REAL)
    audspec_rfilt_sma_de10_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[10].1", REAL)
    audspec_rfilt_sma_de11_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[11].1", REAL)
    audspec_rfilt_sma_de12_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[12].1", REAL)
    audspec_rfilt_sma_de13_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[13].1", REAL)
    audspec_rfilt_sma_de14_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[14].1", REAL)
    audspec_rfilt_sma_de15_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[15].1", REAL)
    audspec_rfilt_sma_de16_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[16].1", REAL)
    audspec_rfilt_sma_de17_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[17].1", REAL)
    audspec_rfilt_sma_de18_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[18].1", REAL)
    audspec_rfilt_sma_de19_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[19].1", REAL)
    audspec_rfilt_sma_de20_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[20].1", REAL)
    audspec_rfilt_sma_de21_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[21].1", REAL)
    audspec_rfilt_sma_de22_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[22].1", REAL)
    audspec_rfilt_sma_de23_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[23].1", REAL)
    audspec_rfilt_sma_de24_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[24].1", REAL)
    audspec_rfilt_sma_de25_1: Mapped[float] = mapped_column("audspec_rfilt_sma_de[25].1", REAL)
    wave_fftmag_fband250_650_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_fband250-650_sma_de.1", REAL)
    wave_fftmag_fband1000_4000_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_fband1000-4000_sma_de.1", REAL)
    wave_fftmag_spectralrolloff25_0_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff25.0_sma_de.1", REAL)
    wave_fftmag_spectralrolloff50_0_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff50.0_sma_de.1", REAL)
    wave_fftmag_spectralrolloff75_0_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff75.0_sma_de.1", REAL)
    wave_fftmag_spectralrolloff90_0_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralrolloff90.0_sma_de.1", REAL)
    wave_fftmag_spectralflux_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralflux_sma_de.1", REAL)
    wave_fftmag_spectralcentroid_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralcentroid_sma_de.1", REAL)
    wave_fftmag_spectralentropy_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralentropy_sma_de.1", REAL)
    wave_fftmag_spectralvariance_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralvariance_sma_de.1", REAL)
    wave_fftmag_spectralskewness_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralskewness_sma_de.1", REAL)
    wave_fftmag_spectralkurtosis_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralkurtosis_sma_de.1", REAL)
    wave_fftmag_spectralslope_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralslope_sma_de.1", REAL)
    wave_fftmag_psysharpness_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_psysharpness_sma_de.1", REAL)
    wave_fftmag_spectralharmonicity_sma_de_1: Mapped[float] = mapped_column(
        "wave_fftmag_spectralharmonicity_sma_de.1", REAL)
    mfcc_sma_de1_1: Mapped[float] = mapped_column("mfcc_sma_de[1].1", REAL)
    mfcc_sma_de2_1: Mapped[float] = mapped_column("mfcc_sma_de[2].1", REAL)
    mfcc_sma_de3_1: Mapped[float] = mapped_column("mfcc_sma_de[3].1", REAL)
    mfcc_sma_de4_1: Mapped[float] = mapped_column("mfcc_sma_de[4].1", REAL)
    mfcc_sma_de5_1: Mapped[float] = mapped_column("mfcc_sma_de[5].1", REAL)
    mfcc_sma_de6_1: Mapped[float] = mapped_column("mfcc_sma_de[6].1", REAL)
    mfcc_sma_de7_1: Mapped[float] = mapped_column("mfcc_sma_de[7].1", REAL)
    mfcc_sma_de8_1: Mapped[float] = mapped_column("mfcc_sma_de[8].1", REAL)
    mfcc_sma_de9_1: Mapped[float] = mapped_column("mfcc_sma_de[9].1", REAL)
    mfcc_sma_de10_1: Mapped[float] = mapped_column("mfcc_sma_de[10].1", REAL)
    mfcc_sma_de11_1: Mapped[float] = mapped_column("mfcc_sma_de[11].1", REAL)
    mfcc_sma_de12_1: Mapped[float] = mapped_column("mfcc_sma_de[12].1", REAL)
    mfcc_sma_de13_1: Mapped[float] = mapped_column("mfcc_sma_de[13].1", REAL)
    mfcc_sma_de14_1: Mapped[float] = mapped_column("mfcc_sma_de[14].1", REAL)
