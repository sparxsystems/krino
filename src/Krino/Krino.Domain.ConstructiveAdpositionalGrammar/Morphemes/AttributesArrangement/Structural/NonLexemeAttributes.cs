using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for non-lexemes.
    /// </summary>
    public class NonLexemeAttributes : EnumGroupBase
    {
        public NonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Affix = new AffixAttributes(this);
        }

        /// <summary>
        /// A grammatical element that is combined with a word, stem, or phrase to produce derived or inflected forms.
        /// </summary>
        public AffixAttributes Affix { get; }
    }
}
