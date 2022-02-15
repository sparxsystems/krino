using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticStructures.Attributes.Sememes
{
    /// <summary>
    /// “Verb aspect” refers to the flow of time.
    /// </summary>
    /// <remarks>
    /// Aspect addresses whether or not the action takes place in a single block of time or if the action is continuous or repeated.
    /// </remarks>
    public class VerbAspectSememes : EnumGroupBase
    {
        public VerbAspectSememes(EnumGroupBase parent) : base(parent)
        {
            Simple = new EnumValue(this);
            Continuous = new EnumValue(this);
            Perfect = new EnumValue(this);
        }

        /// <summary>
        /// Expresses a fact.
        /// </summary>
        /// <remarks>
        /// This aspect is also known as the indefinite aspect.
        /// </remarks>
        public EnumValue Simple { get; }

        /// <summary>
        /// Expresses an ongoing action.
        /// </summary>
        public EnumValue Continuous { get; }

        /// <summary>
        /// Expresses a completed action.
        /// </summary>
        public EnumValue Perfect { get; }

        /// <summary>
        /// Expresses the end of an ongoing action.
        /// </summary>
        public BigInteger ContinousPerfect => Continuous | Perfect;
    }
}
