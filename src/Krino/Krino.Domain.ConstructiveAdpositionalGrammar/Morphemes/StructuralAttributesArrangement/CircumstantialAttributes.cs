using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributesArrangement
{
    /// <summary>
    /// Circumstantial attributes.
    /// </summary>
    public class CircumstantialAttributes : EnumGroupBase
    {
        public CircumstantialAttributes(EnumGroupBase parent) : base(parent)
        {
            Adverb = new AdverbAttributes(this);
        }

        /// <summary>
        /// Circumstantial is an adverb.
        /// </summary>
        public AdverbAttributes Adverb { get; }
    }
}
