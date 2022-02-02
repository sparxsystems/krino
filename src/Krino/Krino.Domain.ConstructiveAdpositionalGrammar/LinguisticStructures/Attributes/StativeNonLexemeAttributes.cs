using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Attributes for stative non-lexemes.
    /// </summary>
    public class StativeNonLexemeAttributes : EnumGroupBase
    {
        public StativeNonLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Prefix = new NounAttributes(this);
            Suffix = new NounAttributes(this);
        }

        /// <summary>
        /// Prefix.
        /// </summary>
        public NounAttributes Prefix { get; }

        /// <summary>
        /// Suffix.
        /// </summary>
        public NounAttributes Suffix { get; }
    }
}
