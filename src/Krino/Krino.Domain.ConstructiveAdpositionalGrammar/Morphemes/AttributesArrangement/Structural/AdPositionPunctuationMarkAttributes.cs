using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.Morphemes.AttributesArrangement.Structural
{
    /// <summary>
    /// Attributes for adposition related punctuation marks.
    /// </summary>
    public class AdPositionPunctuationMarkAttributes : EnumGroupBase
    {
        public AdPositionPunctuationMarkAttributes(EnumGroupBase parent) : base(parent)
        {
            Period = new EnumValue(this);
            QuestionMark = new EnumValue(this);
            ExclamationPoint = new EnumValue(this);
            Comma = new EnumValue(this);
            Semicolon = new EnumValue(this);
            Colon = new EnumValue(this);
        }

        /// <summary>
        /// .
        /// </summary>
        public EnumValue Period { get; }

        /// <summary>
        /// ?
        /// </summary>
        public EnumValue QuestionMark { get; }

        /// <summary>
        /// !
        /// </summary>
        public EnumValue ExclamationPoint { get; }

        /// <summary>
        /// ,
        /// </summary>
        public EnumValue Comma { get; }

        /// <summary>
        /// ;
        /// </summary>
        public EnumValue Semicolon { get; }

        /// <summary>
        /// :
        /// </summary>
        public EnumValue Colon { get; }
    }
}
