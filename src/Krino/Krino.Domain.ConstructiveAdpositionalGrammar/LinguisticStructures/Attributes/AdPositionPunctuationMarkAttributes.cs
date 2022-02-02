using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
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
            Dash = new EnumValue(this);
            Hyphen = new EnumValue(this);
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

        /// <summary>
        /// -
        /// </summary>
        /// <remarks>
        /// To indicate a range, connections or differentiations, such as 1880-1945 or Princeton-New York trains.
        /// </remarks>
        public EnumValue Dash { get; }

        /// <summary>
        /// -
        /// </summary>
        /// <remarks>
        /// A hyphen is used to join two or more words together into a compound term and is not separated by spaces. For example, part-time, back-to-back, well-known.
        /// </remarks>
        public EnumValue Hyphen { get; }
    }
}
