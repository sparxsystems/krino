using Krino.Vertical.Utils.Enums;
using System;
using System.Collections.Generic;
using System.Text;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// Semantic Categories of Subordinating Conjunctions
    /// </summary>
    /// <remarks>
    /// https://www.thoughtco.com/subordinating-conjunction-1692154
    /// </remarks>
    public class SubordinatingConjunctionSememes : EnumGroupBase
    {
        public SubordinatingConjunctionSememes(EnumGroupBase parent) : base(parent)
        {
            Time = new EnumValue(this);
            Concession = new EnumValue(this);
            Comparison = new EnumValue(this);
            Cause = new EnumValue(this);
            Condition = new EnumValue(this);
            Place = new EnumValue(this);
        }

        /// <summary>
        /// Time-related conjunctions establish a period when the main clause will be or was performed.
        /// </summary>
        /// <remarks>
        /// E.g. I will do the dishes after everyone has gone home.
        /// </remarks>
        public EnumValue Time { get; }

        /// <summary>
        /// Concession conjunctions help to redefine the main clause by providing additional context regarding conditions of delivery.
        /// </summary>
        /// <remarks>
        /// E.g. Eliza wrote the Higgins report even though it was assigned to Colonel Pickering.
        /// </remarks>
        public EnumValue Concession { get; }

        /// <summary>
        /// Comparison conjunctions help to establish correlations by providing context for comparison.
        /// </summary>
        /// <remarks>
        /// E.g. Ellen vlogged about the results of the political meeting, in contrast to her arch-enemy who merely blogged.
        /// </remarks>
        public EnumValue Comparison { get; }

        /// <summary>
        /// Cause conjunctions illuminate the reason(s) that the activities of a main clause were performed.
        /// </summary>
        /// <remarks>
        /// E.g. Grant dreamed about cheese because he had eaten so much of it the night before.
        /// </remarks>
        public EnumValue Cause { get; }

        /// <summary>
        /// Condition conjunctions introduce rules under which a main clause performs.
        /// </summary>
        /// <remarks>
        /// E.g. If he's going to be there, I'm not going to the party.
        /// </remarks>
        public EnumValue Condition { get; }

        /// <summary>
        /// Place conjunctions determine where activities might occur.
        /// </summary>
        /// <remarks>
        /// E.g. I will place my conjunction in the sentence wherever I please.
        /// </remarks>
        public EnumValue Place { get; }

    }
}
