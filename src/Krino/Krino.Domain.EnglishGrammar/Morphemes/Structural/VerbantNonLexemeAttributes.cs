using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.EnglishGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for verb non-lexemes.
    /// </summary>
    public class VerbantNonLexemeAttributes : EnumGroupBase
    {
        public VerbantNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new VerbAttributes(this);
            Suffix = new VerbAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public VerbAttributes Prefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public VerbAttributes Suffix { get; }
    }


}
