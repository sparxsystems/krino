using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// A grammatical element that is combined with a word, stem, or phrase to produce derived or inflected forms.
    /// </summary>
    public class AffixAttributes : EnumGroupBase
    {
        public AffixAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new EnumValue(this);
            Infix = new EnumValue(this);
            Suffix = new EnumValue(this);
        }

        /// <summary>
        /// Occurs at beginning of a word.
        /// </summary>
        public EnumValue Prefix { get; }

        /// <summary>
        /// Occurs in the middle of a word - English does not have infixes.
        /// </summary>
        public EnumValue Infix { get; }

        /// <summary>
        /// Occurs at the end of a word.
        /// </summary>
        public EnumValue Suffix { get; }
    }
}
