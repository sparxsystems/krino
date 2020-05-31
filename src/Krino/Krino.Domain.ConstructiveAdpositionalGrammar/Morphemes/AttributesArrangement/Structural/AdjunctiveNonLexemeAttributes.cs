using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for adjunctive non-lexemes.
    /// </summary>
    public class AdjunctiveNonLexemeAttributes : EnumGroupBase
    {
        public AdjunctiveNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new AdjectiveAttributes(this);
            Suffix = new AdjectiveAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public AdjectiveAttributes Prefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public AdjectiveAttributes Suffix { get; }
    }
}
