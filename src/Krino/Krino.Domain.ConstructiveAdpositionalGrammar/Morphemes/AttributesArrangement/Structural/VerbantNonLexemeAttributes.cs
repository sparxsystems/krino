using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for verb non-lexemes.
    /// </summary>
    public class VerbantNonLexemeAttributes : EnumGroupBase
    {
        public VerbantNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            VerbPrefix = new VerbAttributes(this);
            VerbSuffix = new VerbAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public VerbAttributes VerbPrefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public VerbAttributes VerbSuffix { get; }
    }


}
