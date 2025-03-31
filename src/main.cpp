#include <iostream>
#include <stdexcept>
#include <vector>
#include <random>
#include <algorithm>
#include <cassert>
#include <stdio.h>
#include <boost/algorithm/string.hpp>
#include <boost/property_tree/ptree.hpp>
#include <boost/property_tree/ini_parser.hpp>
#include <omp.h>

namespace Random {
	std::random_device seeder; 
	std::mt19937 engine {seeder()};
	
	int Int(int min, int max) {
		std::uniform_int_distribution<int> get{min, max};
		return get(engine);
	}
}

constexpr static int LONG = 1;
constexpr static int SHORT= 1 - LONG;
constexpr static int DEAD = -1;

struct Order {
    int				type;							// 1 - long, 0 - short
		double		enter_price;      // Entry price for the order
    double		size;             // Order size
    double		tp_target;        // Take profit
    double		sl_target;        // Stop loss 
    uint64_t	creation_time;		// Unix timestamp
};

struct ParamSet
{
	long       m_id;
	double     m_tp;
	double     m_sl;
	int        m_instrId;
	double     m_prftRet;
	double     m_commR;
	int        m_rcrdEvery;
};


struct Res
{
	ParamSet   m_prms;
	double     m_pnl;
	double     m_comm;
};


struct Strategy {
	double							m_currPnl		= 0.0;
	double							m_currComm	= 0.0;
	Order               m_currOrd{-1, 0.0, 0.0, 0.0, 0.0, 0};
	ParamSet            m_p;
	FILE*               m_ouF;
	
	void processOrder(double a_px) 
	{
		if (m_currOrd.type == DEAD) [[unlikely]]
			return;

		// Calculate potential profit/loss if order is closed at current price
		bool const isLong = m_currOrd.type == LONG;
		double uPnl				= m_currOrd.size * (a_px - m_currOrd.enter_price)
											* (isLong ? 1.0 : -1.0);
		double amnt				= m_currOrd.size * a_px;
    
		// Check if stop loss is hit
    bool sl_hit = isLong 
			? a_px <= m_currOrd.sl_target 
			:	a_px >= m_currOrd.sl_target;

		// Check if take profit is hit
		bool tp_hit = isLong 
			? a_px >= m_currOrd.tp_target 
			:	a_px <= m_currOrd.tp_target;
		
		assert(!(sl_hit && tp_hit));
		// If either TP or SL is hit, close the position and update balance
		if (sl_hit || tp_hit) 
		{
			// double closing_price = sl_hit ? m_currOrd.sl_target : m_currOrd.tp_target;
			m_currPnl			+= uPnl;
			m_currComm    += amnt * m_p.m_commR;
      
			m_currOrd.type = DEAD;
			return;
		}
    
		// If neither TP nor SL is hit, update stop loss based on position value
		
		if (uPnl > 0) 
		{
			// Move stop loss up to lock in some profit
			double new_sl = a_px + (uPnl * m_p.m_prftRet / amnt)
				            * (isLong ? -1.0 : 1.0);
			m_currOrd.sl_target = isLong 
				? std::max(m_currOrd.sl_target, new_sl)
				: std::min(m_currOrd.sl_target, new_sl);
		}	
		return;
	}
	
	const std::string  m_header = "ts,tp,sl,entPx,entTs,s,pnl,comm,px\n";
	void RecordStats(size_t a_l, double a_px) 
	{
		assert(m_ouF);
		fprintf(m_ouF, "%ld,%f,%f,%f,%ld,%d,%f,%f,%f\n",
			a_l, m_currOrd.tp_target, m_currOrd.sl_target,
			m_currOrd.enter_price, m_currOrd.creation_time,
			m_currOrd.type, m_currPnl, m_currComm, a_px);
	}

	Res DoTest
	(
		std::vector<double>  &a_pxs,
		ParamSet             &a_prms,
		std::string          &a_ouFldr
	) 
	{
		m_p = a_prms;

		m_ouF = fopen((
			a_ouFldr + "Rnd_" + std::to_string(m_p.m_id) + ".csv").c_str(), "w");
		if (!m_ouF) [[unlikely]]
			throw std::runtime_error("Could not open out file!");
		
		fprintf(m_ouF, "%s", m_header.c_str());

		// Process existing orders for each instrument
		for (size_t l = 0; l < a_pxs.size(); l++) 
		{
			double px = a_pxs[l];
			processOrder(px);
        
			if (m_currOrd.type == DEAD) {
				// Create a new random order
				int order_type = Random::Int(0,1);  // 0 for short, 1 for long
        bool const isLong = order_type == LONG;

				double tp_target = px * (1.0 + m_p.m_tp * (isLong ?  1.0 : -1.0));
				double sl_target = px * (1.0 + m_p.m_sl * (isLong ? -1.0 :  1.0));
            
				m_currOrd.type					= order_type;
				m_currOrd.enter_price		= px;
				m_currOrd.size					= 1.0 / px;
				m_currOrd.tp_target			= tp_target;
				m_currOrd.sl_target			= sl_target;
				m_currOrd.creation_time = l;
			}

			if (l % size_t(m_p.m_rcrdEvery) == 0)
				RecordStats(l, px);
		}
		fclose(m_ouF);
		return Res{a_prms, m_currPnl, m_currComm};
	}
};

//---------------------------------------------------------------------------//
// Utils																																		 //
//---------------------------------------------------------------------------//
using namespace std;
vector<int> ParseVeci(string a_in, string a_by = "|")
{
	vector<string> res_str;
	boost::algorithm::split(res_str, a_in, boost::is_any_of(a_by.c_str()));

	vector<int> out;
	out.reserve(res_str.size());
	for (string &str : res_str)
	{
		out.push_back(stoi(str));
	}
	
	return out;
}
vector<double> ParseVecd(string a_in, string a_by = "|")
{
	vector<string> res_str;
	boost::algorithm::split(res_str, a_in, boost::is_any_of(a_by.c_str()));

	vector<double> out;
	out.reserve(res_str.size());
	for (string &str : res_str)
	{
		out.push_back(stod(str));
	}
	
	return out;
}
vector<string> ParseVecs(string a_in, string a_by = "|")
{
	vector<string> res_str;
	boost::algorithm::split(res_str, a_in, boost::is_any_of(a_by.c_str()));

	return res_str;	
}


template<typename T>
size_t FindInVec (vector<T> &a_vec, T a_el, bool trig_exp=false)
{
	size_t res = size_t(find(a_vec.begin(), a_vec.end(), a_el) - a_vec.begin());
	if (res >= a_vec.size() && trig_exp)
	{
		for (T &el : a_vec)
			cout << el << '|';
		cout << '\n' << a_el << '\n';
		throw std::runtime_error(string("Could not find element! ")); 
	}
	return res;
}

int main() {
	//==========================================================================//
	// Read Config																															//
	//==========================================================================//
	using namespace std;

	boost::property_tree::ptree conf;
  boost::property_tree::ini_parser::read_ini("./config.ini", conf);

	string inFileName   =  conf.get<std::string>("InFile");
	string ouFldrName   =  conf.get<std::string>("OutFolder");
	string ouFileName   =  conf.get<std::string>("OutFile");
	
	FILE* inF = fopen(inFileName.c_str(), "r");
	FILE* ouF = fopen(ouFileName.c_str(), "w");
	
	if (inF == nullptr)
		throw runtime_error("Could not open in file");
	if (ouF == nullptr)
	{
		fclose(inF);
		throw runtime_error("Could not open out file");
	}
	
	vector<double> p_tps			= ParseVecd(conf.get<string>("TPs"), "|");
	vector<double> p_sls			= ParseVecd(conf.get<string>("SLs"), "|");
	vector<double> p_prftRet	= ParseVecd(conf.get<string>("PrftRets"), "|");
	double         p_commR		= conf.get<double>("CommR");
	long           p_startR   = conf.get<long>("StartRow");
	long           p_recEvery = conf.get<long>("RecordEvery");

	omp_set_num_threads(conf.get<int>("Threads"));
	
	//=========================================================================//
	// Header scan																														 //
	//=========================================================================//
	constexpr static size_t BuffSz = 150000;
	char buff[BuffSz];
	char* res = fgets(buff, BuffSz, inF);
	if (res == nullptr)
		throw runtime_error("Could not scan header!");
	buff[strlen(buff) - 1] = '\0';
	
	vector<string>  instrs;
	vector<int>     instrIds;
	instrs.reserve(20);
	instrIds.reserve(20);

	vector<string> cols;
  cols.reserve(200);
  boost::split(cols,buff,boost::is_any_of(","));
	
	for (int i = 0; i < int(cols.size()); i++)
	{
		size_t p = cols[i].find("_px");
		if (p != string::npos)
		{
			instrs.push_back(cols[i].substr(0, p));
			instrIds.push_back(i);
		}
	}

	//=========================================================================//
	// Px reading																														   //
	//=========================================================================//
	constexpr static size_t R = 2000000;
	vector<vector<double>> pxVec	(instrs.size());
	for (auto &vec : pxVec)
		vec.reserve(R);
	
	for (size_t l = 0; ; l++)
	{
    char* res = fgets(buff, BuffSz, inF);
		if (res == nullptr) [[unlikely]]
			break;
		buff[strlen(buff) - 1] = '\0';

		if (l < size_t(p_startR))
			continue;
	  
		vector<string> nums;
    nums.reserve(200);
    boost::split(nums,buff,boost::is_any_of(","));
		
		for (size_t i = 0; i < instrs.size() ; i++)
		{
			pxVec[i].push_back(stod(nums[instrIds[i]]));
		}
	}
	
	fclose(inF);
	//=========================================================================//
	// Make out header																												 //
	//=========================================================================//
	string out = "";
	out.reserve(10000);
	out = "id,pnl,comm,commR,tp,sl,instr,prftRet";
	fprintf(ouF, "%s\n", out.c_str());
	
	//=========================================================================//
	// Experiments composition																								 //
	//=========================================================================//
	vector<ParamSet> paramsAll;
	paramsAll.reserve(50000);
	
	long id = 0;
	for (int instId=0; instId < long(instrIds.size()); instId++)
	for (double tp : p_tps)
	for (double sl : p_sls)
	for (double pr : p_prftRet)
	{
		paramsAll.emplace_back(id++, tp, sl, instId, pr, p_commR, p_recEvery);
	}

	vector<Res> ress(paramsAll.size());
	#pragma omp parallel for
	for (ParamSet &prm: paramsAll)
	{
		Strategy strat;
		Res res = strat.DoTest(pxVec[prm.m_instrId], prm, ouFldrName);
		ress[prm.m_id] = res;
	}
	
	// record results
	for (Res &res : ress)
	{
		ParamSet p = res.m_prms;
		fprintf(ouF, "%ld,%f,%f,%f,%f,%f,%s,%f\n",
			p.m_id, res.m_pnl, res.m_comm, p.m_commR, p.m_tp, p.m_sl,
			instrs[p.m_instrId].c_str(), p.m_prftRet
		);
	}

	fclose(ouF);	

}
