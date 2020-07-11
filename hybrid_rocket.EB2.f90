
program HybridRocket


      implicit none

!     --- Parameters for integration---
  
      integer nmax,n,i,j,k,l,m
      parameter(nmax=100000)
     
      real*8 dum, dt
      real*8 pi
!     real*8 eps
!------------------------------------------------------------------------------
!     Input parameters

!     --- Oxidizer ---
!     l_dot_fuel(t)=alpha_fuel*a_fuel*go(t)**n_fuel
!     Coeffient_a_for_regression_rate
      real*8 a_fuel
      real*8 alpha_fuel_ini
	  real*8 alpha_fuel_upper
	  real*8 alpha_fuel_lower

!     Coeffient_n_for_regression_rate
      real*8 n_fuel

!     Oxidizer_density Initial_oxidizer_mass-flow_rate
      real*8 dens_oxi, m_dot_oxi_ini
     
!     --- Fuel ---
!     ---Initial-port-radius  Fuel_density Fuel_length
      real*8 r_port, dens_fuel, l_fuel_ini
	  real*8 l_fuel(0:nmax)
!     --- Chamber ---
!     Pressure@chamber Burning_time Chamber_internal_radius
      real*8 pc_bar, t_burn, r_fuel

!.kana.1312
      real*8 t_stop_burn,t_rest_burn,t_tot
      integer igon
	  real*8 t_not_burn
!.kana.1312_en

!     Nozzle_expansion_ratio
      real*8 epsilon

!------------------------------------------------------------------------------
!     --- Calculated parameters ---
      real*8 go_ini
      real*8 vol_oxi,vol_fuel
      real*8 l_dot_fuel_ini 
      real*8 m_prop_ini, m_dot_prop_ini 
      real*8 m_dot_fuel_ini,of_ini
      real*8 area_flow_ini

!------------------------------------------------------------------------------
!     --- Parameters for time integration ---
      real*8 time(0:nmax),time_oxi(0:nmax)

!     --- Parameters for rocket weight ---
      real*8 m_prop(0:nmax)
      real*8 m_oxi(0:nmax), mn_oxi(0:nmax)
      real*8 m_fuel(0:nmax), mn_fuel(0:nmax)

!     --- Parameters for flow in nozzle ---
      real*8 m_dot_oxi(0:nmax),m_dot_fuel(0:nmax)
      real*8 m_dot_prop(0:nmax), of(0:nmax)
!     real*8 r_port, rn_port(1:nmax)
      real*8 l_dot_fuel(0:nmax), rn_dot_port(1:nmax)
      real*8 area_flow(0:nmax)
      real*8 go(0:nmax), go_n(0:nmax)
	  
!	  real*8 alpha_fuel(0:nmax)
	  real*8 alpha_fuel_temp(0:nmax)

!------------------------------------------------------------------------------
!     --- Rocket thrust parameters ---
!     Pressure@nozzle_exit Sound_velocity@nozzle_exit
      real*8 pe_ini, son_ini

!     Mach_number@nozzle_exit Initial_specific_impulse 
      real*8 mach_ini, isp_ini

!     Inital_thrus_coefficient Area@nozzle_throat Area@nozzle_exit
      real*8 cf_ini, area_t, area_e

!     Efficient_of_Cf Efficien_of_Isp nozzle_expansion_coefficient
      real*8 eita_cf, eita_isp, ramda

!     --- Rocket structure parameters ---
!     Chamber&Oxidizer_reservoir_internal_radius throat_radius exit_radius
      real*8 r_t, r_e

!     Oxidizer_weight Fuel_weight Chamber-weight Oxidizer-resevoir-weight
      real*8 m_ch, m_res, m_noz

!     Weight_ratio(Rocket_total_weight=Propellant_weight/m_ratio+Payload_weight)
      real*8  m_ratio

!     Payload_weight Rocket_structure_weight
      real*8 m_pay, m_str

!     Nozzle_length Chamber_lenght Oxidizer_reservoir_cylinder_lenght
      real*8 l_noz, l_ch, l_res_cyl, l_res

!     Engine_lenght Rocket_total_lenght 
      real*8 l_tot_en,  l_tot

!     Lenght_ratio(l_tot=l_tot_en/l_ratio)
      real*8 l_ratio

!     Oxideizer_reservoir_volume Chamber_volume
      real*8 v_res, v_ch

!     Pressure@Chamber Pressure@Oxidizer_reservoir
      real*8 p_ch, p_res

!     Yield_stress@Chamber Yield_stress@Oxideizer_reservoir
      real*8 sty_ch, sty_res

!     Oxidizer_density Fuel_density Chamber_density Oxideizer_reservoir_density
      real*8 dens_ch, dens_res

!     Nozzle_expantion_ratio Safe_factor
      real*8 sf

!     Nozzle_compression_angle Nozzle_expansion_angle
      real*8 theta1, theta2

!     Thickness @Chamber Thickness@Oxidizer_reservoir
      real*8 t_ch, t_res

!     Rocket_radius Rocket_reference_area Rocket_wet_area
      real*8 r_tot, ref_area, ref_area_wet

!     Propellant_weight Rocket_total_weight
      real*8 m_tot(0:nmax),  ld

!------------------------------------------------------------------------------
!     Parameters from NASA CEA 

!     Pressure@nozzle_exit Sound_velocity@nozzle_exit
      real*8 pe, son
	  real*8 pen(0:nmax), sonn(0:nmax)

!     Mach_number@nozzle_exit Specific_impulse 
      real*8 mach, isp(0:nmax), isp_th(0:nmax), isp0
	  real*8 machn(0:nmax)

!     Thrus_coefficient
      real*8 cf, cf_th(0:nmax)
!------------------------------------------------------------------------------
!     Parameters for Trajectory calculation

!     --- Parameters at i step ---

!     Atomosphre_pressure Altitude Velocity 
      real*8 p_atm1, hei1, vel1, acc1, ro1
!.kana
      real*8 acc1c
      real*8 cross1,cron2,vel1c,vel1h
      real*8 veln2c,vel2c(0:nmax),cro2(0:nmax)
      real*8 accn2c,acc2c(0:nmax)
      real*8 cromax,accmin,acc,vel
      real*8 theta_ini
	  real*8 theta_l1, theta_ln2, theta_l2(0:nmax)
	  real*8 theta_p1, theta_pn2, theta_p2(0:nmax)
!.kana.e
!.yoda.e

!     Kinematic_viscosity Atomosphere_Temperature
      real*8 nyu1, t_atm1

!     Mach_number Sound_velocity
      real*8 mach1, sound1

!     --- Parameters at i+1 step ---

!     Atomosphre_pressure Altitude Velocity Acceleration Kinematic_viscosity
      real*8 p_atm2(0:nmax), hein2, hei2(0:nmax), veln2, vel2(0:nmax)
      real*8  accn2, acc2(0:nmax)

!     Kinematic_viscosity Atomosphere_Temperature
      real*8 nyu2(0:nmax), t_atm2(0:nmax), ro2(0:nmax)

!     Mach_number Sound_velocity
      real*8 mach2, sound2(0:nmax)

!     --- Drag parameters of S-520 ---

!     Drag_coefficient Frictional_drag Pressure_drag
      real*8 cd_s520, cft_s520, cft_s520_ref, cdp_s520

!     Reynolds_number
      real*8 reynolds
      real*8 rey(0:nmax)
!     --- Drag parameters of designed rocket ---

!     Drag_coefficient Frictional_drag Pressure_drag
      real*8 cft_des(0:nmax), cdp_des(0:nmax)

!     Drag Drag@sea_level
      real*8 drag(0:nmax), drag0

!     --- Parameters of designed rocket's thrust ---

!     Thrust Exhaust_velocity@nozzle_exit Burning_time
      real*8 thrust(0:nmax), ue
	  real*8 uen(0:nmax)
	  real*8 thrust1n(0:nmax), thrust2n(0:nmax)

!     Efficiency_of_Thrust
      real*8 eita_T

!     --- Parameters for fitness ---
      real*8 altmax, accmax, time1, time2, time100, time3, time4, timeobs

      real*8 cross_range1, cross_range2, cross_range3, cross_range4
      real*8 duration_time1, duration_time2, duration_time3, duration_time4
      real*8 cross_range_LT, duration_time_LT
	  real*8 d_presmax, d_pres(0:nmax), tw_ratio 

!yoda
!     --- Parameters for lift ---

!     Density of air 
      real*8 dens_atml(0:nmax)

!yoda.e

!------------------------------------------------------------------------------
!yoda
!     Parameters for orbital analysis

!     --- Center of gravity ---
      real*8 cg(0:nmax)

!     --- Nozzle surface area ---
      real*8 a_noz
      real*8 a_noz_e
      real*8 a_noz_i

!     --- Nozzle mass ---
      real*8 mass_noz_e
      real*8 mass_noz_i

!     --- Nozzle CoG length ---
      real*8 cgl_noz_e
      real*8 cgl_noz_i

!     --- Nozzle CoG ---
      real*8 cge
      real*8 cgi

!     --- Nozzle projected area ---
      real*8 pa_noz

!     --- Nose corn ratio ---
      real*8 nc_ratio

!     --- Nose corn length ---
      real*8 l_nose

!     --- Nose corn projected area ---
      real*8 pa_nose

!     --- Length of payload ---
      real*8 l_pay

!     --- Weight of wall ---
      real*8 m_wall

!     --- Each of CoG ---
      real*8 cg_ch(0:nmax)
      real*8 cg_res(0:nmax)
      real*8 cg_pay
      real*8 cg_nose

!     --- Angle of attack (deg) ---
!      real*8 theta_p0
!      real*8 theta_p(0:nmax)

!     --- Center of Pressure ---
      real*8 cp

!     --- Moment of Inertia ---
      real*8 mi(0:nmax)

!     --- Lift coefficient ---
      real*8 cln(0:nmax)

!     --- Absolute velocity ---
      real*8 vela(0:nmax)

!     --- Lift ---
      real*8 lift0
      real*8 lift(0:nmax)

!     --- X component thrust ---
      real*8 thrust_x(0:nmax)

!     --- Y component thrust ---
      real*8 thrust_y(0:nmax)

!     --- Acceleration of theta_p ---
      real*8 acc_theta_p1, acc_theta_pn2
      real*8 acc_theta_p2(0:nmax)
	  
!     --- theta_p and omega1 2 ---
      real*8 omega1
	  real*8 omega2(0:nmax)
	  real*8 omegan2

!     --- Theta gap ---
      real*8 gap_theta
      real*8 gap_theta0
	  real*8 gap_theta1
	  real*8 gap(0:nmax)
	  real*8 cosgap, singap

!     --- CFT ---
      real*8 cft_s520i(0:nmax)
	  
      real*8 check
	  real*8 l_factor
	  real*8 alpha_judge
	  
	  
!yoda.e

!------------------------------------------------------------------------------

!     --- Parameters for Standard Atmosphere ---

!     Density_change_ratio_with_altitude Atomosphere_Density Gravity
      real*8 alpha, dens_atm0, gravity

!------------------------------------------------------------------------------
      pi  = 4.d0*atan(1.d0)
      gravity   = 9.80665
      dens_atm0 = 1.48
!------------------------------------------------------------------------------
!     --- Formats ---
10    format('',12A16)
20    format('',12f16.6)
30    format(f16.7)
40    format(f16.3)

      open(110,file='dv_EB2.inp',form='formatted')
        read(110,30) m_dot_oxi_ini
		read(110,30) of_ini
		read(110,30) t_burn
		read(110,30) t_stop_burn
        read(110,30) t_rest_burn
		read(110,30) l_fuel_ini 
		read(110,30) r_port
!		read(110,30) go_ini
        read(110,30) pc_bar
		read(110,30) epsilon
        read(110,30) theta_ini
        read(110,30) m_pay
      close(110)	
	    
	  open(111,file='cons_EB2.inp',form='formatted')
        read(111,*)  
        read(111,40) dt
        read(111,*)  
        read(111,*)  
        read(111,30) dens_oxi
        read(111,*)  
        read(111,*)  
        read(111,30) a_fuel
        read(111,30) n_fuel
        read(111,30) dens_fuel
		read(111,30) alpha_fuel_upper
		read(111,30) alpha_fuel_lower
        read(111,*) 
        read(111,30) eita_cf
        read(111,30) eita_isp
        read(111,*)  
        read(111,30) sf
        read(111,30) theta1
        read(111,30) theta2
        read(111,30) sty_ch
        read(111,30) dens_ch
        read(111,30) sty_res
        read(111,30) dens_res
        read(111,30) m_ratio
        read(111,30) l_ratio
        read(111,*)  
        read(111,30) alpha
        read(111,*) 
        read(111,30) eita_T
      close(111)

!     --- Calculated parameters ---
      area_flow_ini   = pi*(r_port)**2.d0
	  go_ini          = m_dot_oxi_ini/area_flow_ini
      l_dot_fuel_ini  = a_fuel*pc_bar**n_fuel
	  m_dot_fuel_ini  = m_dot_oxi_ini/of_ini
      m_dot_prop_ini  = m_dot_oxi_ini+m_dot_fuel_ini
	  
!------- Engine Simulation -------
!      --- Initial conditions ---	
      n             = 0
      time(0)       = 0.d0
      time_oxi(0)   = 0.d0
	  area_flow(0)  = area_flow_ini 
	  go(0)         = go_ini
      go_n(0)       = 0.d0
	  l_dot_fuel(0) = l_dot_fuel_ini
	  m_dot_fuel(0) = m_dot_fuel_ini
      m_dot_oxi(0)  = m_dot_oxi_ini
	  m_dot_prop(0) = m_dot_prop_ini
	  l_fuel(0)     = l_fuel_ini
      of(0)         = of_ini
	  mn_fuel(0) = 0.d0
	  check = 0.d0
	  l_factor = 1.d0
	  
	  if(t_burn.lt.t_stop_burn)then 
        t_tot=t_burn
       else
        t_tot=t_burn+t_rest_burn-t_stop_burn
       endif

      do n=1,nmax
        time_oxi(n) = time_oxi(n-1) + dt
		
	  igon=0
      if(time_oxi(n).le.t_stop_burn)igon=1
      if(time_oxi(n).ge.t_rest_burn)igon=1
      if(time_oxi(n).le.t_tot .and.  igon.eq.1)then
		 m_dot_oxi(n)   = m_dot_oxi_ini
         mn_oxi(n)      = mn_oxi(n-1)+&
                          &(m_dot_oxi(n-1)+m_dot_oxi(n))*dt/2.d0
      else
         m_dot_oxi(n)  = 0.d0
         mn_oxi(n)     = mn_oxi(n-1)
      endif
      enddo
         m_oxi(0) = mn_oxi(nmax)

!     --- Time Integration ---
      do n=1,nmax
        time(n) = time(n-1) + dt
		
	  igon=0
      if(time(n).le.t_stop_burn)igon=1
      if(time(n).ge.t_rest_burn)igon=1
	  if(time(n).le.t_tot .and.  igon.eq.1)then
	     l_dot_fuel(n) = a_fuel*pc_bar**n_fuel
         l_fuel(n)     = l_fuel_ini-l_dot_fuel(n-1)*time(n)
         m_dot_oxi(n)  = m_dot_oxi_ini
         area_flow(n)  = pi*(r_port)**2.0
         go(n)         = m_dot_oxi(n)/area_flow(n)
		 of(n)         = of_ini
         m_dot_fuel(n) = m_dot_oxi_ini/of(n)
         m_dot_prop(n) = m_dot_oxi(n)+m_dot_fuel(n)
         m_oxi(n)      = m_oxi(n-1)-&
                             &(m_dot_oxi(n-1)+m_dot_oxi(n))*dt/2.d0
         mn_fuel(n)    = mn_fuel(n-1)+&
                           &(m_dot_fuel(n-1)+m_dot_fuel(n))*dt/2.d0
	  else
            l_dot_fuel(n) = 0.d0
            m_dot_oxi(n)  = 0.d0
            go(n)         = 0.d0
            go_n(n)       = 0.d0
            m_dot_fuel(n) = 0.d0
            m_dot_prop(n) = 0.d0
            m_oxi(n)      = m_oxi(n-1)
            mn_fuel(n)    = mn_fuel(n-1)
          endif
      enddo
	  
!     --- Fuel weight calculation
      m_fuel(0)   = mn_fuel(nmax)
      do i=1,nmax
        m_fuel(i) = m_fuel(i-1)-&
                   &(m_dot_fuel(i-1)+m_dot_fuel(i))*dt/2.d0
      enddo

!     --- Chamber internal radius calculation ---
      r_fuel    = (m_fuel(0)/(dens_fuel*pi*l_fuel_ini)&
                   &+r_port**2.d0)**0.5d0

!     --- Propellant weight calculation ---
      do i=0,nmax
        m_prop(i) = m_oxi(i)+m_fuel(i)
      enddo
	  
!     --- Output ---
      open(2,file='engine-out_EB2.dat',form='formatted')
        write(2,10)'time',&
		  &'propellant_mass',&
          &'m_dot_oxi',&
		  &'m_dot_fuel',&
		  &'O/F',&
          &'m_oxi',&
		  &'m_fuel',&
          &'m_dot_prop',&
		  &'Go',&
          &'l_dot_fuel',&
		  &'flow_area',&
		  &'l_fuel'
        do i=0,nmax
          write(2,20)time(i),&
		  &m_prop(i),&
		  &m_dot_oxi(i),&
		  &m_dot_fuel(i),&
		  &of(i),&
		  &m_oxi(i),&
		  &m_fuel(i),&
		  &m_dot_prop(i),&
		  &go(i),&
          &l_dot_fuel(i),&
		  &area_flow(i),&
		  &l_fuel(i)		  
        enddo
      close(2)







end program HybridRocket