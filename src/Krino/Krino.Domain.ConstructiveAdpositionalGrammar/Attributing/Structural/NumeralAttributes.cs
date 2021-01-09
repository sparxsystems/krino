using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Attributing.Structural
{
    /// <summary>
    /// Attribute for numerals.
    /// </summary>
    /// <remarks>
    /// Numerals can be used in sveral categories e.g. in adjectives, nouns and adverbs.
    /// </remarks>
    public class NumeralAttributes : EnumGroupBase
    {
        public NumeralAttributes(EnumGroupBase parent) : base(parent)
        {
            Cardinal = new EnumValue(this);
            Ordinal = new EnumValue(this);
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
