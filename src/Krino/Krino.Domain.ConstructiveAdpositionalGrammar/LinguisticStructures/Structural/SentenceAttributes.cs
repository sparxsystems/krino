using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Structural
{
    public class SentenceAttributes : EnumGroupBase
    {
        public SentenceAttributes(EnumGroupBase parent) : base(parent)
        {
            SimpleSentence = new EnumValue(this);
            ComplexSentence = new EnumValue(this);
            CompoundSentence = new CompoundSentenceAttributes(this);
            CompoundComplexSentence = new EnumValue(this);
        }

        /// <summary>
        /// Contains just one independent clause.
        /// </summary>
        public EnumValue SimpleSentence { get; }

        /// <summary>
        /// Contains two or more independent clauses.
        /// </summary>
        public EnumValue ComplexSentence { get; }

        /// <summary>
        /// Contains one independent clause and one dependent clause.
        /// </summary>
        public CompoundSentenceAttributes CompoundSentence { get; }

        /// <summary>
        /// Contains at least two independent clauses and one dependent clause.
        /// </summary>
        public EnumValue CompoundComplexSentence { get; }
    }
}
