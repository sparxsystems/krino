using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for adjunctive non-lexemes.
    /// </summary>
    public class AdjunctiveNonLexemeAttributes : EnumGroupBase
    {
        public AdjunctiveNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new EnumValue(this);
            Suffix = new EnumValue(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public EnumValue Prefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public EnumValue Suffix { get; }
    }
}
