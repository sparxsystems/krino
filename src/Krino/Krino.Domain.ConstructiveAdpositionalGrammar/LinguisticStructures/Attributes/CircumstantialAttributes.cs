using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
{
    /// <summary>
    /// Circumstantial attributes.
    /// </summary>
    public class CircumstantialAttributes : EnumGroupBase
    {
        public CircumstantialAttributes(EnumGroupBase parent) : base(parent)
        {
            Free = new CircumstantialLexemeAttributes(this);
            Bound = new CircumstantialNonLexemeAttributes(this);
        }

        /// <summary>
        /// Lexeme.
        /// </summary>
        public CircumstantialLexemeAttributes Free { get; }

        /// <summary>
        /// Non-lexeme.
        /// </summary>
        public CircumstantialNonLexemeAttributes Bound { get; }
    }
}
