using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for adposition non-lexemes.
    /// </summary>
    public class AdPositionNonLexemeAttributes : EnumGroupBase
    {
        public AdPositionNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            PunctuationMark = new AdPositionPunctuationMarkAttributes(this);
        }

        /// <summary>
        /// Punction mark like . ! ? , ; : etc.
        /// </summary>
        public AdPositionPunctuationMarkAttributes PunctuationMark { get; }
    }
}
