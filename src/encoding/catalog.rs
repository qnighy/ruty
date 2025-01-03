use crate::encoding::EncodingImpl;

use crate::encoding::impls::{
    Ascii8bitImpl, Cesu8Impl, UsAsciiImpl, Utf16BeImpl, Utf16BomImpl, Utf16LeImpl, Utf32BeImpl,
    Utf32BomImpl, Utf32LeImpl, Utf8Impl,
};
#[allow(unused)]
use crate::encoding::impls::{Utf16Impl, Utf32Impl};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub enum Encoding {
    // The big three
    ASCII_8BIT,
    UTF_8,
    US_ASCII,

    // Unicode-based
    UTF8_MAC,
    UTF8_DoCoMo,
    UTF8_KDDI,
    UTF8_SoftBank,
    CESU_8,
    UTF_7,
    UTF_16BE,
    UTF_16LE,
    UTF_16,
    UTF_32BE,
    UTF_32LE,
    UTF_32,

    // One-byte
    ISO_8859_1,
    ISO_8859_2,
    ISO_8859_3,
    ISO_8859_4,
    ISO_8859_5,
    ISO_8859_6,
    Windows_1256,
    ISO_8859_7,
    ISO_8859_8,
    Windows_1255,
    ISO_8859_9,
    ISO_8859_10,
    ISO_8859_11,
    TIS_620,
    Windows_874,
    ISO_8859_13,
    ISO_8859_14,
    ISO_8859_15,
    ISO_8859_16,
    KOI8_R,
    KOI8_U,
    Windows_1250,
    Windows_1251,
    Windows_1252,
    Windows_1253,
    Windows_1254,
    Windows_1257,
    IBM437,
    IBM720,
    IBM737,
    IBM775,
    CP850,
    IBM852,
    CP852,
    IBM855,
    CP855,
    IBM857,
    IBM860,
    IBM861,
    IBM862,
    IBM863,
    IBM864,
    IBM865,
    IBM866,
    IBM869,
    Windows_1258,
    GB1988,
    macCentEuro,
    macCroatian,
    macCyrillic,
    macGreek,
    macIceland,
    macRoman,
    macRomania,
    macThai,
    macTurkish,
    macUkraine,

    // EBCDIC
    IBM037,

    // Multilingual
    Emacs_Mule,
    stateless_ISO_2022_JP,
    stateless_ISO_2022_JP_KDDI,

    // Japanese - EUC-JP
    EUC_JP,
    eucJP_ms,
    CP51932,
    EUC_JIS_2004,

    // Japanese - Shift_JIS
    Shift_JIS,
    Windows_31J,
    MacJapanese,
    SJIS_DoCoMo,
    SJIS_KDDI,
    SJIS_SoftBank,

    // Japanese - ISO-2022-JP
    ISO_2022_JP,
    ISO_2022_JP_2,
    CP50220,
    CP50221,
    ISO_2022_JP_KDDI,

    // Korean - EUC-KR
    EUC_KR,
    GB2312,
    GB12345,
    CP949,

    // Chinese (Taiwan) - EUC-TW
    EUC_TW,

    // Chinese (Taiwan) - Big5
    Big5,
    CP950,
    Big5_HKSCS,
    CP951,
    Big5_UAO,

    // Chinese (PRC) - GBK
    GBK,
    GB18030,
}

impl Encoding {
    pub(in crate::encoding) fn aliases_impl(&self) -> &'static [&'static str] {
        match *self {
            // The big three
            Encoding::ASCII_8BIT => &["ASCII-8BIT", "BINARY"],
            Encoding::UTF_8 => &["UTF-8", "CP65001"],
            Encoding::US_ASCII => &["US-ASCII", "ASCII", "ANSI_X3.4-1968", "646"],

            // Unicode-based
            Encoding::UTF8_MAC => &["UTF8-MAC", "UTF-8-MAC", "UTF-8-HFS"],
            Encoding::UTF8_DoCoMo => &["UTF8-DoCoMo"],
            Encoding::UTF8_KDDI => &["UTF8-KDDI"],
            Encoding::UTF8_SoftBank => &["UTF8-SoftBank"],
            Encoding::CESU_8 => &["CESU-8"],
            Encoding::UTF_7 => &["UTF-7", "CP65000"],
            Encoding::UTF_16BE => &["UTF-16BE", "UCS-2BE"],
            Encoding::UTF_16LE => &["UTF-16LE"],
            Encoding::UTF_16 => &["UTF-16"],
            Encoding::UTF_32BE => &["UTF-32BE", "UCS-4BE"],
            Encoding::UTF_32LE => &["UTF-32LE", "UCS-4LE"],
            Encoding::UTF_32 => &["UTF-32"],

            // One-byte
            Encoding::ISO_8859_1 => &["ISO-8859-1", "ISO8859-1"],
            Encoding::ISO_8859_2 => &["ISO-8859-2", "ISO8859-2"],
            Encoding::ISO_8859_3 => &["ISO-8859-3", "ISO8859-3"],
            Encoding::ISO_8859_4 => &["ISO-8859-4", "ISO8859-4"],
            Encoding::ISO_8859_5 => &["ISO-8859-5", "ISO8859-5"],
            Encoding::ISO_8859_6 => &["ISO-8859-6", "ISO8859-6"],
            Encoding::Windows_1256 => &["Windows-1256", "CP1256"],
            Encoding::ISO_8859_7 => &["ISO-8859-7", "ISO8859-7"],
            Encoding::ISO_8859_8 => &["ISO-8859-8", "ISO8859-8"],
            Encoding::Windows_1255 => &["Windows-1255", "CP1255"],
            Encoding::ISO_8859_9 => &["ISO-8859-9", "ISO8859-9"],
            Encoding::ISO_8859_10 => &["ISO-8859-10", "ISO8859-10"],
            Encoding::ISO_8859_11 => &["ISO-8859-11", "ISO8859-11"],
            Encoding::TIS_620 => &["TIS-620"],
            Encoding::Windows_874 => &["Windows-874", "CP874"],
            Encoding::ISO_8859_13 => &["ISO-8859-13", "ISO8859-13"],
            Encoding::ISO_8859_14 => &["ISO-8859-14", "ISO8859-14"],
            Encoding::ISO_8859_15 => &["ISO-8859-15", "ISO8859-15"],
            Encoding::ISO_8859_16 => &["ISO-8859-16", "ISO8859-16"],
            Encoding::KOI8_R => &["KOI8-R", "CP878"],
            Encoding::KOI8_U => &["KOI8-U"],
            Encoding::Windows_1250 => &["Windows-1250", "CP1250"],
            Encoding::Windows_1251 => &["Windows-1251", "CP1251"],
            Encoding::Windows_1252 => &["Windows-1252", "CP1252"],
            Encoding::Windows_1253 => &["Windows-1253", "CP1253"],
            Encoding::Windows_1254 => &["Windows-1254", "CP1254"],
            Encoding::Windows_1257 => &["Windows-1257", "CP1257"],
            Encoding::IBM437 => &["IBM437", "CP437"],
            Encoding::IBM720 => &["IBM720", "CP720"],
            Encoding::IBM737 => &["IBM737", "CP737"],
            Encoding::IBM775 => &["IBM775", "CP775"],
            Encoding::CP850 => &["CP850", "IBM850"],
            Encoding::IBM852 => &["IBM852"],
            Encoding::CP852 => &["CP852"],
            Encoding::IBM855 => &["IBM855"],
            Encoding::CP855 => &["CP855"],
            Encoding::IBM857 => &["IBM857", "CP857"],
            Encoding::IBM860 => &["IBM860", "CP860"],
            Encoding::IBM861 => &["IBM861", "CP861"],
            Encoding::IBM862 => &["IBM862", "CP862"],
            Encoding::IBM863 => &["IBM863", "CP863"],
            Encoding::IBM864 => &["IBM864", "CP864"],
            Encoding::IBM865 => &["IBM865", "CP865"],
            Encoding::IBM866 => &["IBM866", "CP866"],
            Encoding::IBM869 => &["IBM869", "CP869"],
            Encoding::Windows_1258 => &["Windows-1258", "CP1258"],
            Encoding::GB1988 => &["GB1988"],
            Encoding::macCentEuro => &["macCentEuro"],
            Encoding::macCroatian => &["macCroatian"],
            Encoding::macCyrillic => &["macCyrillic"],
            Encoding::macGreek => &["macGreek"],
            Encoding::macIceland => &["macIceland"],
            Encoding::macRoman => &["macRoman"],
            Encoding::macRomania => &["macRomania"],
            Encoding::macThai => &["macThai"],
            Encoding::macTurkish => &["macTurkish"],
            Encoding::macUkraine => &["macUkraine"],

            // EBCDIC
            Encoding::IBM037 => &["IBM037", "ebcdic-cp-us"],

            // Multilingual
            Encoding::Emacs_Mule => &["Emacs-Mule"],
            Encoding::stateless_ISO_2022_JP => &["stateless-ISO-2022-JP"],
            Encoding::stateless_ISO_2022_JP_KDDI => &["stateless-ISO-2022-JP-KDDI"],

            // Japanese - EUC-JP
            Encoding::EUC_JP => &["EUC-JP", "eucJP"],
            Encoding::eucJP_ms => &["eucJP-ms", "euc-jp-ms"],
            Encoding::CP51932 => &["CP51932"],
            Encoding::EUC_JIS_2004 => &["EUC-JIS-2004", "EUC-JISX0213"],

            // Japanese - Shift_JIS
            Encoding::Shift_JIS => &["Shift_JIS"],
            Encoding::Windows_31J => &["Windows-31J", "csWindows31J", "SJIS", "PCK"],
            Encoding::MacJapanese => &["MacJapanese", "MacJapan"],
            Encoding::SJIS_DoCoMo => &["SJIS-DoCoMo"],
            Encoding::SJIS_KDDI => &["SJIS-KDDI"],
            Encoding::SJIS_SoftBank => &["SJIS-SoftBank"],

            // Japanese - ISO-2022-JP
            Encoding::ISO_2022_JP => &["ISO-2022-JP", "ISO2022-JP"],
            Encoding::ISO_2022_JP_2 => &["ISO-2022-JP-2", "ISO2022-JP2"],
            Encoding::CP50220 => &["CP50220"],
            Encoding::CP50221 => &["CP50221"],
            Encoding::ISO_2022_JP_KDDI => &["ISO-2022-JP-KDDI"],

            // Korean - EUC-KR
            Encoding::EUC_KR => &["EUC-KR", "eucKR"],
            Encoding::GB2312 => &["GB2312", "EUC-CN", "eucCN"],
            Encoding::GB12345 => &["GB12345"],
            Encoding::CP949 => &["CP949"],

            // Chinese (Taiwan) - EUC-TW
            Encoding::EUC_TW => &["EUC-TW", "eucTW"],

            // Chinese (Taiwan) - Big5
            Encoding::Big5 => &["Big5"],
            Encoding::CP950 => &["CP950"],
            Encoding::Big5_HKSCS => &["Big5-HKSCS", "Big5-HKSCS:2008"],
            Encoding::CP951 => &["CP951"],
            Encoding::Big5_UAO => &["Big5-UAO"],

            // Chinese (PRC) - GBK
            Encoding::GBK => &["GBK", "CP936"],
            Encoding::GB18030 => &["GB18030"],
        }
    }

    pub(in crate::encoding) fn encoding_class(&self) -> EncodingClass {
        match *self {
            // The big three
            Encoding::ASCII_8BIT => EncodingClass::ASCII_8BIT,
            Encoding::UTF_8 => EncodingClass::UTF_8,
            Encoding::US_ASCII => EncodingClass::US_ASCII,

            // Unicode-based
            Encoding::UTF8_MAC => EncodingClass::UTF_8,
            Encoding::UTF8_DoCoMo => EncodingClass::UTF_8,
            Encoding::UTF8_KDDI => EncodingClass::UTF_8,
            Encoding::UTF8_SoftBank => EncodingClass::UTF_8,
            Encoding::CESU_8 => EncodingClass::CESU_8,
            Encoding::UTF_7 => EncodingClass::UTF_7,
            Encoding::UTF_16BE => EncodingClass::UTF_16BE,
            Encoding::UTF_16LE => EncodingClass::UTF_16LE,
            Encoding::UTF_16 => EncodingClass::UTF_16,
            Encoding::UTF_32BE => EncodingClass::UTF_32BE,
            Encoding::UTF_32LE => EncodingClass::UTF_32LE,
            Encoding::UTF_32 => EncodingClass::UTF_32,

            // One-byte
            Encoding::ISO_8859_1 => EncodingClass::ISO_8859_1,
            Encoding::ISO_8859_2 => EncodingClass::ISO_8859_2,
            Encoding::ISO_8859_3 => EncodingClass::ISO_8859_3,
            Encoding::ISO_8859_4 => EncodingClass::ISO_8859_4,
            Encoding::ISO_8859_5 => EncodingClass::ISO_8859_5,
            Encoding::ISO_8859_6 => EncodingClass::ISO_8859_6,
            Encoding::Windows_1256 => EncodingClass::ISO_8859_6,
            Encoding::ISO_8859_7 => EncodingClass::ISO_8859_7,
            Encoding::ISO_8859_8 => EncodingClass::ISO_8859_8,
            Encoding::Windows_1255 => EncodingClass::ISO_8859_8,
            Encoding::ISO_8859_9 => EncodingClass::ISO_8859_9,
            Encoding::ISO_8859_10 => EncodingClass::ISO_8859_10,
            Encoding::ISO_8859_11 => EncodingClass::ISO_8859_11,
            Encoding::TIS_620 => EncodingClass::ISO_8859_11,
            Encoding::Windows_874 => EncodingClass::ISO_8859_11,
            Encoding::ISO_8859_13 => EncodingClass::ISO_8859_13,
            Encoding::ISO_8859_14 => EncodingClass::ISO_8859_14,
            Encoding::ISO_8859_15 => EncodingClass::ISO_8859_15,
            Encoding::ISO_8859_16 => EncodingClass::ISO_8859_16,
            Encoding::KOI8_R => EncodingClass::KOI8_R,
            Encoding::KOI8_U => EncodingClass::KOI8_U,
            Encoding::Windows_1250 => EncodingClass::Windows_1250,
            Encoding::Windows_1251 => EncodingClass::Windows_1251,
            Encoding::Windows_1252 => EncodingClass::Windows_1252,
            Encoding::Windows_1253 => EncodingClass::Windows_1253,
            Encoding::Windows_1254 => EncodingClass::Windows_1254,
            Encoding::Windows_1257 => EncodingClass::Windows_1257,
            Encoding::IBM437 => EncodingClass::ASCII_8BIT,
            Encoding::IBM720 => EncodingClass::ASCII_8BIT,
            Encoding::IBM737 => EncodingClass::ASCII_8BIT,
            Encoding::IBM775 => EncodingClass::ASCII_8BIT,
            Encoding::CP850 => EncodingClass::ASCII_8BIT,
            Encoding::IBM852 => EncodingClass::ASCII_8BIT,
            Encoding::CP852 => EncodingClass::ASCII_8BIT,
            Encoding::IBM855 => EncodingClass::ASCII_8BIT,
            Encoding::CP855 => EncodingClass::ASCII_8BIT,
            Encoding::IBM857 => EncodingClass::ASCII_8BIT,
            Encoding::IBM860 => EncodingClass::ASCII_8BIT,
            Encoding::IBM861 => EncodingClass::ASCII_8BIT,
            Encoding::IBM862 => EncodingClass::ASCII_8BIT,
            Encoding::IBM863 => EncodingClass::ASCII_8BIT,
            Encoding::IBM864 => EncodingClass::ASCII_8BIT,
            Encoding::IBM865 => EncodingClass::ASCII_8BIT,
            Encoding::IBM866 => EncodingClass::ASCII_8BIT,
            Encoding::IBM869 => EncodingClass::ASCII_8BIT,
            Encoding::Windows_1258 => EncodingClass::ASCII_8BIT,
            Encoding::GB1988 => EncodingClass::ASCII_8BIT,
            Encoding::macCentEuro => EncodingClass::ASCII_8BIT,
            Encoding::macCroatian => EncodingClass::ASCII_8BIT,
            Encoding::macCyrillic => EncodingClass::ASCII_8BIT,
            Encoding::macGreek => EncodingClass::ASCII_8BIT,
            Encoding::macIceland => EncodingClass::ASCII_8BIT,
            Encoding::macRoman => EncodingClass::ASCII_8BIT,
            Encoding::macRomania => EncodingClass::ASCII_8BIT,
            Encoding::macThai => EncodingClass::ASCII_8BIT,
            Encoding::macTurkish => EncodingClass::ASCII_8BIT,
            Encoding::macUkraine => EncodingClass::ASCII_8BIT,

            // EBCDIC
            Encoding::IBM037 => EncodingClass::IBM037,

            // Multilingual
            Encoding::Emacs_Mule => EncodingClass::Emacs_Mule,
            Encoding::stateless_ISO_2022_JP => EncodingClass::Emacs_Mule,
            Encoding::stateless_ISO_2022_JP_KDDI => EncodingClass::ISO_2022_JP,

            // Japanese - EUC-JP
            Encoding::EUC_JP => EncodingClass::EUC_JP,
            Encoding::eucJP_ms => EncodingClass::EUC_JP,
            Encoding::CP51932 => EncodingClass::EUC_JP,
            Encoding::EUC_JIS_2004 => EncodingClass::EUC_JP,

            // Japanese - Shift_JIS
            Encoding::Shift_JIS => EncodingClass::Shift_JIS,
            Encoding::Windows_31J => EncodingClass::Windows_31J,
            Encoding::MacJapanese => EncodingClass::Shift_JIS,
            Encoding::SJIS_DoCoMo => EncodingClass::Windows_31J,
            Encoding::SJIS_KDDI => EncodingClass::Windows_31J,
            Encoding::SJIS_SoftBank => EncodingClass::Windows_31J,

            // Japanese - ISO-2022-JP
            Encoding::ISO_2022_JP => EncodingClass::ISO_2022_JP,
            Encoding::ISO_2022_JP_2 => EncodingClass::ISO_2022_JP,
            Encoding::CP50220 => EncodingClass::ISO_2022_JP,
            Encoding::CP50221 => EncodingClass::ISO_2022_JP,
            Encoding::ISO_2022_JP_KDDI => EncodingClass::ISO_2022_JP,

            // Korean - EUC-KR
            Encoding::EUC_KR => EncodingClass::EUC_KR,
            Encoding::GB2312 => EncodingClass::EUC_KR,
            Encoding::GB12345 => EncodingClass::EUC_KR,
            Encoding::CP949 => EncodingClass::CP949,

            // Chinese (Taiwan) - EUC-TW
            Encoding::EUC_TW => EncodingClass::EUC_TW,

            // Chinese (Taiwan) - Big5
            Encoding::Big5 => EncodingClass::Big5,
            Encoding::CP950 => EncodingClass::Big5,
            Encoding::Big5_HKSCS => EncodingClass::Big5_HKSCS,
            Encoding::CP951 => EncodingClass::Big5_HKSCS,
            Encoding::Big5_UAO => EncodingClass::Big5_UAO,

            // Chinese (PRC) - GBK
            Encoding::GBK => EncodingClass::GBK,
            Encoding::GB18030 => EncodingClass::GB18030,
        }
    }

    pub(in crate::encoding) fn encoding_impl(&self) -> &'static dyn EncodingImpl {
        match self.encoding_class() {
            // The big three
            EncodingClass::ASCII_8BIT => &Ascii8bitImpl,
            EncodingClass::UTF_8 => &Utf8Impl,
            EncodingClass::US_ASCII => &UsAsciiImpl,

            EncodingClass::CESU_8 => &Cesu8Impl,
            EncodingClass::UTF_7 => &Ascii8bitImpl,
            EncodingClass::UTF_16BE => &Utf16BeImpl,
            EncodingClass::UTF_16LE => &Utf16LeImpl,
            EncodingClass::UTF_16 => &Utf16BomImpl,
            EncodingClass::UTF_32BE => &Utf32BeImpl,
            EncodingClass::UTF_32LE => &Utf32LeImpl,
            EncodingClass::UTF_32 => &Utf32BomImpl,
            EncodingClass::ISO_8859_1 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_2 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_3 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_4 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_5 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_6 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_7 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_8 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_9 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_10 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_11 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_13 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_14 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_15 => &Ascii8bitImpl,
            EncodingClass::ISO_8859_16 => &Ascii8bitImpl,
            EncodingClass::KOI8_R => &Ascii8bitImpl,
            EncodingClass::KOI8_U => &Ascii8bitImpl,
            EncodingClass::Windows_1250 => &Ascii8bitImpl,
            EncodingClass::Windows_1251 => &Ascii8bitImpl,
            EncodingClass::Windows_1252 => &Ascii8bitImpl,
            EncodingClass::Windows_1253 => &Ascii8bitImpl,
            EncodingClass::Windows_1254 => &Ascii8bitImpl,
            EncodingClass::Windows_1257 => &Ascii8bitImpl,
            EncodingClass::IBM037 => &Ascii8bitImpl,
            EncodingClass::Emacs_Mule => &Ascii8bitImpl,
            EncodingClass::EUC_JP => &Ascii8bitImpl,
            EncodingClass::Shift_JIS => &Ascii8bitImpl,
            EncodingClass::Windows_31J => &Ascii8bitImpl,
            EncodingClass::ISO_2022_JP => &Ascii8bitImpl,
            EncodingClass::EUC_KR => &Ascii8bitImpl,
            EncodingClass::CP949 => &Ascii8bitImpl,
            EncodingClass::EUC_TW => &Ascii8bitImpl,
            EncodingClass::Big5 => &Ascii8bitImpl,
            EncodingClass::Big5_HKSCS => &Ascii8bitImpl,
            EncodingClass::Big5_UAO => &Ascii8bitImpl,
            EncodingClass::GBK => &Ascii8bitImpl,
            EncodingClass::GB18030 => &Ascii8bitImpl,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
pub(in crate::encoding) enum EncodingClass {
    // The big three
    ASCII_8BIT,
    UTF_8,
    US_ASCII,

    // Unicode-based
    CESU_8,
    UTF_7,
    UTF_16BE,
    UTF_16LE,
    UTF_16,
    UTF_32BE,
    UTF_32LE,
    UTF_32,

    // One-byte
    ISO_8859_1,
    ISO_8859_2,
    ISO_8859_3,
    ISO_8859_4,
    ISO_8859_5,
    ISO_8859_6,
    ISO_8859_7,
    ISO_8859_8,
    ISO_8859_9,
    ISO_8859_10,
    ISO_8859_11,
    ISO_8859_13,
    ISO_8859_14,
    ISO_8859_15,
    ISO_8859_16,
    KOI8_R,
    KOI8_U,
    Windows_1250,
    Windows_1251,
    Windows_1252,
    Windows_1253,
    Windows_1254,
    Windows_1257,

    // EBCDIC
    IBM037,

    // Multilingual
    Emacs_Mule,

    // Japanese - EUC-JP
    EUC_JP,

    // Japanese - Shift_JIS
    Shift_JIS,
    Windows_31J,

    // Japanese - ISO-2022-JP
    ISO_2022_JP,

    // Korean - EUC-KR
    EUC_KR,
    CP949,

    // Chinese (Taiwan) - EUC-TW
    EUC_TW,

    // Chinese (Taiwan) - Big5
    Big5,
    Big5_HKSCS,
    Big5_UAO,

    // Chinese (PRC) - GBK
    GBK,
    GB18030,
}
