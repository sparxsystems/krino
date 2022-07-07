using System.Collections.Generic;

namespace Krino.Domain.ConstructiveGrammar.LinguisticStructures
{
    public interface IClause : ILinguisticStructure, IPhraseItem
    {
        IWord Conjunction { get; set; }

        ISubject Subject { get; set; }

        IPredicate Predicate { get; set; }

        IEnumerable<IClause> DependentClauses { get;}

        /// <summary>
        /// Returns this clause without independent clauses.
        /// </summary>
        IClause IndependentClause { get; }
    }
}
