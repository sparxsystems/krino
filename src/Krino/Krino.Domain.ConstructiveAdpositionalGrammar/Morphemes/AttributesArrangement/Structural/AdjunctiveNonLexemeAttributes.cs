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
            AdjextivePrefix = new AdjectiveAttributes(this);
            AdjectiveSuffix = new AdjectiveAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public AdjectiveAttributes AdjextivePrefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public AdjectiveAttributes AdjectiveSuffix { get; }
    }
}
