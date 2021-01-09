using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for statives.
    /// </summary>
    public class StativeAttributes : EnumGroupBase
    {
        public StativeAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new StativeLexemeAttributes(this);
            NonLexeme = new StativeNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public StativeLexemeAttributes Lexeme { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public StativeNonLexemeAttributes NonLexeme { get; }
    }
}
