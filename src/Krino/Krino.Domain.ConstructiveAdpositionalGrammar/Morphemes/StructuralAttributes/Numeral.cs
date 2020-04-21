using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.StructuralAttributes
{
    /// <summary>
    /// Attribute for numerals.
    /// </summary>
    /// <remarks>
    /// Numerals can be used in sveral categories e.g. in adjectives, nouns and adverbs.
    /// </remarks>
    public class Numeral : EnumGroupBase
    {
        public Numeral(EnumGroupBase parent, int localPosition) : base(parent, localPosition, 2)
        {
            Cardinal = new EnumValue(this, 1);
            Ordinal = new EnumValue(this, 2);
        }

        /// <summary>
        /// Expresses the count.
        /// </summary>
        public EnumValue Cardinal { get; }

        /// <summary>
        /// Expresses the position or order e.g. second place.
        /// </summary>
        public EnumValue Ordinal { get; }
    }
}
