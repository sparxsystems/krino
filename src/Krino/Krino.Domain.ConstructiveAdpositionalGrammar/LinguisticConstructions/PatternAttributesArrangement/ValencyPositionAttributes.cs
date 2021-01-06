using Krino.Vertical.Utils.Enums;
using System.Numerics;

namespace Krino.Domain.ConstructiveAdpositionalGrammar.LinguisticConstructions.PatternAttributesArrangement
{
    /// <summary>
    /// Attributes indicating which valency positions are saturated.
    /// </summary>
    public class ValencyPositionAttributes : EnumGroupBase
    {
        public ValencyPositionAttributes(PatternAttributes parent) : base(parent)
        {
            First = new EnumValue(this);
            Second = new EnumValue(this);
            Third = new EnumValue(this);
            Fourth = new EnumValue(this);
            Fifth = new EnumValue(this);
        }

        /// <summary>
        /// First valency position is saturated.
        /// </summary>
        public EnumValue First { get; }

        /// <summary>
        /// Second valency position is saturated.
        /// </summary>
        public EnumValue Second { get; }

        /// <summary>
        /// Third valency position is saturated.
        /// </summary>
        public EnumValue Third { get; }

        /// <summary>
        /// Fourth valency position is saturated.
        /// </summary>
        public EnumValue Fourth { get; }

        /// <summary>
        /// Fifth valency position is saturated.
        /// </summary>
        public EnumValue Fifth { get; }

        /// <summary>
        /// Returns the valency position.
        /// </summary>
        /// <param name="attributes"></param>
        /// <returns></returns>
        public int GetValencyPosition(BigInteger attributes)
        {
            if (First.IsIn(attributes))
            {
                return 1;
            }
            if (Second.IsIn(attributes))
            {
                return 2;
            }
            if (Third.IsIn(attributes))
            {
                return 3;
            }
            if (Fourth.IsIn(attributes))
            {
                return 4;
            }
            if (Fifth.IsIn(attributes))
            {
                return 5;
            }

            return 0;
        }
    }
}
