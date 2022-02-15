﻿using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes
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
        /// Contains two or more independent clauses.
        /// </summary>
        public ComplexSentenceAttributes Complex { get; }

        /// <summary>
        /// Contains one independent clause and one dependent clause.
        /// </summary>
        public EnumValue Compound { get; }

        /// <summary>
        /// Contains at least two independent clauses and one dependent clause.
        /// </summary>
        public EnumValue CompoundComplex { get; }


        public EnumValue Declarative { get; }

        /// <summary>
        /// Question.
        /// </summary>
        public EnumValue Interrogative { get; }

        public EnumValue Imperative { get; }
        
        public EnumValue Exclamative { get; }

    }
}