using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Attributing.Structural
{
    /// <summary>
    /// Attributes for circumstantial lexemes.
    /// </summary>
    public class CircumstantialLexemeAttributes : EnumGroupBase
    {
        public CircumstantialLexemeAttributes(EnumGroupBase parent) : base(parent)
        {
            Preposition = new EnumValue(this);
            Postposition = new EnumValue(this);
            Adverb = new AdverbAttributes(this);
        }

        /// <summary>
        /// Circumstantial is a preposition.
        /// </summary>
        /// <remarks>
        /// Circumstantial can be an adposition with the preposition morpheme.
        /// E.g. I read the book in the room.
        /// </remarks>
        public EnumValue Preposition { get; }

        /// <summary>
        /// Circumstantial is a postposition.
        /// </summary>
        public EnumValue Postposition { get; }

        /// <summary>
        /// Circumstantial is an adverb.
        /// </summary>
        public AdverbAttributes Adverb { get; }
    }
}
