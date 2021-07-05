using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.EnglishGrammar.Morphemes.Semantic
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
        /// Combines the perfect and the progressive to refer to the completed portion of a continuous action.
        /// </summary>
        public BigInteger ContinousPerfect => Continuous | Perfect;
    }
}
