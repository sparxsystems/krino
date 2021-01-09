using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for adposition non-lexemes.
    /// </summary>
    public class AdPositionNonLexemeAttributes : EnumGroupBase
    {
        public AdPositionNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            PunctuationMark = new EnumValue(this);
        }

        /// <summary>
        /// Punction mark like . ! ? , ; : etc.
        /// </summary>
        public EnumValue PunctuationMark { get; }
    }
}
