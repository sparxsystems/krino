using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for stative non-lexemes.
    /// </summary>
    public class StativeNonLexemeAttributes : EnumGroupBase
    {
        public StativeNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            NounPrefix = new NounAttributes(this);
            NounSuffix = new NounAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public NounAttributes NounPrefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public NounAttributes NounSuffix { get; }
    }
}
