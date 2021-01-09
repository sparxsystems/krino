using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.Structural
{
    /// <summary>
    /// Circumstantial attributes.
    /// </summary>
    public class CircumstantialAttributes : EnumGroupBase
    {
        public CircumstantialAttributes(EnumGroupBase parent) : base(parent)
        {
            Lexeme = new EnumValue(this);
            NonLexeme = new CircumstantialNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public EnumValue Lexeme { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public CircumstantialNonLexemeAttributes NonLexeme { get; }
    }
}
