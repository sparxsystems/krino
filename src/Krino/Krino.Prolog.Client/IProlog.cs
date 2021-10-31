using System.Collections.Generic;
using System.Threading.Tasks;

namespace Krino.Prolog.Client
{
    public interface IProlog
    {
        Task Add(string statement);
        Task Add(IEnumerable<string> statements);

        Task Remove(string statement);
        Task Remove(IEnumerable<string> statements);
        Task Clear();

        Task<bool> Evaluate(string statement);
    }
}
