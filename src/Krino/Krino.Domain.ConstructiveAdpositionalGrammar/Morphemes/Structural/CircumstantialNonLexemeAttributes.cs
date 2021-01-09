using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for circumstantial non-lexemes.
    /// </summary>
    public class CircumstantialNonLexemeAttributes : EnumGroupBase
    {
        public CircumstantialNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            AdverbPrefix = new EnumValue(this);
            AdverbSuffix = new EnumValue(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public EnumValue AdverbPrefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public EnumValue AdverbSuffix { get; }
    }
}
