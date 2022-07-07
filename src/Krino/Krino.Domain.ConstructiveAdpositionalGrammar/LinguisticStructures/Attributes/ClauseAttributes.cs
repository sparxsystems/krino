using Krino.Vertical.Utils.Enums;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures.Attributes
{
    public class ClauseAttributes : EnumGroupBase
    {
        public ClauseAttributes(EnumGroupBase parent) : base(parent)
        {
            Dependent = new DependentClauseAttributes(this);
            Independent = new EnumValue(this);

            Premis = new EnumValue(this);
            Conclusion = new EnumValue(this);
        }

        /// <summary>
        /// Starts with a subordinating conjunction.
        /// </summary>
        public DependentClauseAttributes Dependent { get; }
        public EnumValue Independent { get; }


        public EnumValue Premis { get; }
        public EnumValue Conclusion { get; }
    }
}
