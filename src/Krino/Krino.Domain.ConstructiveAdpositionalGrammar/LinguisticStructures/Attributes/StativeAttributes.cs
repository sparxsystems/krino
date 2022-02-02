using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Attributes for statives.
    /// </summary>
    public class StativeAttributes : EnumGroupBase
    {
        public StativeAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new StativeLexemeAttributes(this);
            Bound = new StativeNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public StativeLexemeAttributes Free { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public StativeNonLexemeAttributes Bound { get; }
    }
}
