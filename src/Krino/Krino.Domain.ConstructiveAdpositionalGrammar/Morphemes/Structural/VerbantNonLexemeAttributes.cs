using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for verb non-lexemes.
    /// </summary>
    public class VerbantNonLexemeAttributes : EnumGroupBase
    {
        public VerbantNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new EnumValue(this);
            Suffix = new EnumValue(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public EnumValue Prefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public EnumValue Suffix { get; }
    }


}
