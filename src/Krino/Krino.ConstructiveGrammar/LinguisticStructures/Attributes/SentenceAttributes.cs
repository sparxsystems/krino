using Krino.Vertical.Utils.Enums;

namespace Krino.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class SentenceAttributes : EnumGroupBase
    {
        public SentenceAttributes(EnumGroupBase parent) : base(parent)
        {
            Simple = new EnumValue(this);
            Complex = new ComplexSentenceAttributes(this);
            Compound = new EnumValue(this);
            CompoundComplex = new EnumValue(this);

            Declarative = new EnumValue(this);
            Interrogative = new EnumValue(this);
            Imperative = new EnumValue(this);
            Exclamative = new EnumValue(this);
        }

        /// <summary>
        /// Contains just one independent clause.
        /// </summary>
        public EnumValue Simple { get; }

        /// <summary>
        /// Contains one independent clause and at least one dependent clause.
        /// </summary>
        public ComplexSentenceAttributes Complex { get; }

        /// <summary>
        /// Contains two or more independent clauses.
        /// </summary>
        public EnumValue Compound { get; }

        /// <summary>
        /// Contains at least two independent clauses and one dependent clause.
        /// </summary>
        public EnumValue CompoundComplex { get; }


        public EnumValue Declarative { get; }

        /// <summary>
        /// Expresses question.
        /// </summary>
        public EnumValue Interrogative { get; }

        /// <summary>
        /// Expresses command.
        /// </summary>
        public EnumValue Imperative { get; }

        /// <summary>
        /// Expresses emotion.
        /// </summary>
        public EnumValue Exclamative { get; }

    }
}
