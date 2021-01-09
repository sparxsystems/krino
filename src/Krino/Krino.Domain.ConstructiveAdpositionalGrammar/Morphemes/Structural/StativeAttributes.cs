using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Attributes for statives.
    /// </summary>
    public class StativeAttributes : EnumGroupBase
    {
        public StativeAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new EnumValue(this);
            NonLexeme = new StativeNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public EnumValue Lexeme { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public StativeNonLexemeAttributes NonLexeme { get; }
    }
}
