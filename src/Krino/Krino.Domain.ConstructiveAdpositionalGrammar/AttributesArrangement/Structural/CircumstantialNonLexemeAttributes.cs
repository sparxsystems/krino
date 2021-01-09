using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for circumstantial non-lexemes.
    /// </summary>
    public class CircumstantialNonLexemeAttributes : EnumGroupBase
    {
        public CircumstantialNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            AdverbPrefix = new AdverbAttributes(this);
            AdverbSuffix = new AdverbAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public AdverbAttributes AdverbPrefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public AdverbAttributes AdverbSuffix { get; }
    }
}
