      program HybridRocket
!******************************************************************************

      implicit none

!     --- Parameters for integration---
  
      integer nmax,n,i,j,k,l,m
      parameter(nmax=100000)
     
      real dum, dt
      real*8 pi
!     real*8 eps
!------------------------------------------------------------------------------
!     Input parameters

!     --- Oxidizer ---
!     l_dot_fuel(t)=alpha_fuel*a_fuel*go(t)**n_fuel
!     Coeffient_a_for_regression_rate
      real a_fuel
      real alpha_fuel_ini
	  real alpha_fuel_upper
	  real alpha_fuel_lower

!     Coeffient_n_for_regression_rate
      real n_fuel

!     Oxidizer_density Initial_oxidizer_mass-flow_rate
      real dens_oxi, m_dot_oxi_ini
     
!     --- Fuel ---
!　　 Initial-port-radius 　Fuel_density Fuel_length
      real r_port,   dens_fuel,   l_fuel

!     --- Chamber ---
!     Pressure@chamber Burning_time Chamber_internal_radius
      real pc_bar, t_burn, r_fuel

!      多数回燃焼 20200623 消去
!.kana.1312
      real t_stop_burn,t_rest_burn,t_tot
      integer igon
	  real t_not_burn
!.kana.1312_en

!       A-SOFT 時間比
!!      real time_ratio_1
!!      real time_ratio_2
!!      real time_ratio_3
!!	  real ttt, t1, t2, t3
!     real t1, t2, t3
!	  real t4 !再着火時の燃焼時間

!     Nozzle_expansion_ratio
      real epsilon

!------------------------------------------------------------------------------
!     --- Calculated parameters ---
      real go_ini
      real vol_oxi,vol_fuel
      real l_dot_fuel_ini 
      real m_prop_ini, m_dot_prop_ini 
      real m_dot_fuel_ini,of_ini
!	  real of4 !再着火時のO/F
      real area_flow_ini

!------------------------------------------------------------------------------
!     --- Parameters for time integration ---
      real time(0:nmax),time_oxi(0:nmax)

!     --- Parameters for rocket weight ---
      real m_prop(0:nmax)
      real m_oxi(0:nmax), mn_oxi(0:nmax)
      real m_fuel(0:nmax), mn_fuel(0:nmax)

!     --- Parameters for flow in nozzle ---
      real m_dot_oxi(0:nmax),m_dot_fuel(0:nmax)
      real m_dot_prop(0:nmax), of(0:nmax)
      real r_port(0:nmax), rn_port(1:nmax)
      real l_dot_fuel(0:nmax), rn_dot_port(1:nmax)
      real area_flow(0:nmax)
      real go(0:nmax), go_n(0:nmax)
	  
!	  real alpha_fuel(0:nmax)
	  real alpha_fuel_temp(0:nmax)

!------------------------------------------------------------------------------
!     --- Rocket thrust parameters ---
!     Pressure@nozzle_exit Sound_velocity@nozzle_exit
      real pe_ini, son_ini

!     Mach_number@nozzle_exit Initial_specific_impulse 
      real mach_ini, isp_ini

!     Inital_thrus_coefficient Area@nozzle_throat Area@nozzle_exit
      real cf_ini, area_t, area_e

!     Efficient_of_Cf Efficien_of_Isp nozzle_expansion_coefficient
      real eita_cf, eita_isp, ramda

!     --- Rocket structure parameters ---
!     Chamber&Oxidizer_reservoir_internal_radius throat_radius exit_radius
      real r_t, r_e

!     Oxidizer_weight Fuel_weight Chamber-weight Oxidizer-resevoir-weight
      real m_ch, m_res, m_noz

!     Weight_ratio(Rocket_total_weight=Propellant_weight/m_ratio+Payload_weight)
      real  m_ratio

!     Payload_weight Rocket_structure_weight
      real m_pay, m_str

!     Nozzle_length Chamber_lenght Oxidizer_reservoir_cylinder_lenght
      real l_noz, l_ch, l_res_cyl, l_res

!     Engine_lenght Rocket_total_lenght 
      real l_tot_en,  l_tot

!     Lenght_ratio(l_tot=l_tot_en/l_ratio)
      real l_ratio

!     Oxideizer_reservoir_volume Chamber_volume
      real v_res, v_ch

!     Pressure@Chamber Pressure@Oxidizer_reservoir
      real p_ch, p_res

!     Yield_stress@Chamber Yield_stress@Oxideizer_reservoir
      real sty_ch, sty_res

!     Oxidizer_density Fuel_density Chamber_density Oxideizer_reservoir_density
      real dens_ch, dens_res

!     Nozzle_expantion_ratio Safe_factor
      real sf

!     Nozzle_compression_angle Nozzle_expansion_angle
      real theta1, theta2

!     Thickness @Chamber Thickness@Oxidizer_reservoir
      real t_ch, t_res

!     Rocket_radius Rocket_reference_area Rocket_wet_area
      real r_tot, ref_area, ref_area_wet

!     Propellant_weight Rocket_total_weight
      real m_tot(0:nmax),  ld

!------------------------------------------------------------------------------
!     Parameters from NASA CEA 

!     Pressure@nozzle_exit Sound_velocity@nozzle_exit
      real pe, son
	  real pen(0:nmax), sonn(0:nmax)

!     Mach_number@nozzle_exit Specific_impulse 
      real mach, isp(0:nmax), isp_th(0:nmax), isp0
	  real machn(0:nmax)

!     Thrus_coefficient
      real cf, cf_th(0:nmax)
!------------------------------------------------------------------------------
!     Parameters for Trajectory calculation

!     --- Parameters at i step ---

!     Atomosphre_pressure Altitude Velocity 
      real p_atm1, hei1, vel1, acc1, ro1
!.kana
      real acc1c
      real cross1,cron2,vel1c,vel1h
      real veln2c,vel2c(0:nmax),cro2(0:nmax)
      real accn2c,acc2c(0:nmax)
      real cromax,accmin,acc,vel
      real theta_ini
	  real theta_l1, theta_ln2, theta_l2(0:nmax)
	  real theta_p1, theta_pn2, theta_p2(0:nmax)
!.kana.e
!.yoda.e

!     Kinematic_viscosity Atomosphere_Temperature
      real nyu1, t_atm1

!     Mach_number Sound_velocity
      real mach1, sound1

!     --- Parameters at i+1 step ---

!     Atomosphre_pressure Altitude Velocity Acceleration Kinematic_viscosity
      real p_atm2(0:nmax), hein2, hei2(0:nmax), veln2, vel2(0:nmax)
      real  accn2, acc2(0:nmax)

!     Kinematic_viscosity Atomosphere_Temperature
      real nyu2(0:nmax), t_atm2(0:nmax), ro2(0:nmax)

!     Mach_number Sound_velocity
      real mach2, sound2(0:nmax)

!     --- Drag parameters of S-520 ---

!     Drag_coefficient Frictional_drag Pressure_drag
      real cd_s520, cft_s520, cft_s520_ref, cdp_s520

!     Reynolds_number
      real reynolds
      real rey(0:nmax)
!     --- Drag parameters of designed rocket ---

!     Drag_coefficient Frictional_drag Pressure_drag
      real cft_des(0:nmax), cdp_des(0:nmax)

!     Drag Drag@sea_level
      real drag(0:nmax), drag0

!     --- Parameters of designed rocket's thrust ---

!     Thrust Exhaust_velocity@nozzle_exit Burning_time
      real thrust(0:nmax), ue
	  real uen(0:nmax)
	  real thrust1n(0:nmax), thrust2n(0:nmax)

!     Efficiency_of_Thrust
      real eita_T

!     --- Parameters for fitness ---
      real altmax, accmax, time1, time2, time100, time3, time4, timeobs

      real cross_range1, cross_range2, cross_range3, cross_range4
      real duration_time1, duration_time2, duration_time3, duration_time4
      real cross_range_LT, duration_time_LT
	  real d_presmax, d_pres(0:nmax), tw_ratio !最大動圧、動圧、推重比

!yoda
!     --- Parameters for lift ---

!     Density of air 
      real dens_atml(0:nmax)

!yoda.e

!------------------------------------------------------------------------------
!yoda
!     Parameters for orbital analysis

!     --- Center of gravity ---
      real cg(0:nmax)

!     --- Nozzle surface area ---
      real a_noz
      real a_noz_e
      real a_noz_i

!     --- Nozzle mass ---
      real mass_noz_e
      real mass_noz_i

!     --- Nozzle CoG length ---
      real cgl_noz_e
      real cgl_noz_i

!     --- Nozzle CoG ---
      real cge
      real cgi

!     --- Nozzle projected area ---
      real pa_noz

!     --- Nose corn ratio ---
      real nc_ratio

!     --- Nose corn length ---
      real l_nose

!     --- Nose corn projected area ---
      real pa_nose

!     --- Length of payload ---
      real l_pay

!     --- Weight of wall ---
      real m_wall

!     --- Each of CoG ---
      real cg_ch(0:nmax)
      real cg_res(0:nmax)
      real cg_pay
      real cg_nose

!     --- Angle of attack (deg) ---
!      real theta_p0
!      real theta_p(0:nmax)

!     --- Center of Pressure ---
      real cp

!     --- Moment of Inertia ---
      real mi(0:nmax)

!     --- Lift coefficient ---
      real cln(0:nmax)

!     --- Absolute velocity ---
      real vela(0:nmax)

!     --- Lift ---
      real lift0
      real lift(0:nmax)

!     --- X component thrust ---
      real thrust_x(0:nmax)

!     --- Y component thrust ---
      real thrust_y(0:nmax)

!     --- Acceleration of theta_p ---
      real acc_theta_p1, acc_theta_pn2
      real acc_theta_p2(0:nmax)
	  
!     --- theta_p and omega1 2 ---
      real omega1
	  real omega2(0:nmax)
	  real omegan2

!     --- Theta gap ---
      real gap_theta
      real gap_theta0
	  real gap_theta1
	  real gap(0:nmax)
	  real cosgap, singap

!     --- CFT ---
      real cft_s520i(0:nmax)
	  
!     --- チェック ---
      real check
	  real l_factor
	  real alpha_judge
	  
	  
!yoda.e

!------------------------------------------------------------------------------

!     --- Parameters for Standard Atmosphere ---

!     Density_change_ratio_with_altitude Atomosphere_Density Gravity
      real alpha, dens_atm0, gravity

!------------------------------------------------------------------------------
      pi  = 4.d0*atan(1.d0)
      gravity   = 9.80665
      dens_atm0 = 1.48
!      eps = 0.000000000000001
!------------------------------------------------------------------------------
!     --- Formats ---
10    format('',12A16)
20    format('',12f16.6)
30    format(f16.7)
40    format(f16.3)
!------------------------------------------------------------------------------
      open(110,file='dv_EB1.inp',form='formatted')
        read(110,30) m_dot_oxi_ini
!        read(110,30) m_dot_oxi_ini_2
!        read(110,30) m_dot_oxi_ini_3
!		read(110,30) m_dot_oxi_ini_4
        read(110,30) of_ini
!       read(110,30) of2
!      read(110,30) of3
!		read(110,30) of4
        read(110,30) t_burn
		read(110,30) t_stop_burn
        read(110,30) t_rest_burn
!       read(110,30) t2
!        read(110,30) t3
!        read(110,30) t_not_burn
!		read(110,30) t4
        read(110,30) go_ini
        read(110,30) pc_bar
!        read(110,30) alpha_fuel_ini
        read(110,30) epsilon
        read(110,30) theta_ini
        read(110,30) m_pay
!!        read(110,30) t_burn
!!        read(110,30) time_ratio_1
!!        read(110,30) time_ratio_2
!!        read(110,30) time_ratio_3
!     	read(110,30) t_stop_burn

!.kana.1312_en
      close(110)
!       t_rest_burn=t_rest_burn+t_stop_burn

      open(111,file='cons_EB1.inp',form='formatted')
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

	  
	  
!------------------------------------------------------------------------------
!     --- Calculated parameters ---
!      r_port_ini = (m_dot_oxi_ini/(pi*go_ini))**0.5
!      l_fuel = m_dot_oxi_ini_1/(2*pi*dens_fuel*r_port_ini*alpha_fuel_ini*a_fuel*go_ini**(n_fuel)*of1)
!      r_port_ini,l_fuelは設計変数とした
!        print *, "l_fuel=",l_fuel
!        print *, "m_dot_oxi_ini_1=",m_dot_oxi_ini_1
!        print *, "r_port_ini=",r_port_ini
!        print *, "of1=",of1
!        print *, "alpha_fuel_ini=",alpha_fuel_ini
      area_flow_ini   = pi*(r_port)**2.d0
      l_dot_fuel      = a_fuel*pc_bar**n_fuel
!        print *, "l_dot_fuel_ini=",l_dot_fuel_ini
      m_dot_fuel_ini  = m_dot_oxi_ini/of_ini
      m_dot_prop_ini  = m_dot_oxi_ini+m_dot_fuel_ini
!------------------------------------------------------------------------------
!     --- Engine Simulation ---

!     --- Initial conditions ---
      n             = 0
      time(0)       = 0.d0
      time_oxi(0)   = 0.d0
      m_dot_oxi(0)  = m_dot_oxi_ini
      m_dot_fuel(0) = m_dot_fuel_ini
      of(0)         = of_ini
      m_dot_prop(0) = m_dot_prop_ini
      l_dot_fuel(0) = l_dot_fuel
      go(0)         = go_ini
      go_n(0)       = 0.d0
	  mn_fuel(0) = 0.d0
      area_flow(0)  = area_flow_ini
!     alpha_fuel_temp(0) = m_dot_oxi(0)/(2*pi*dens_fuel*l_fuel*a_fuel*of_ini*(m_dot_oxi(0)/pi)**n_fuel*r_port(0)**(-2*n_fuel+1))
!        print *, "alpha_fuel_temp(",n,")=",alpha_fuel_temp(n)!!!!!!!!!!
!        print *, "time(0)", time(0)!!!!!!!!!!
      check = 0.d0
	  l_factor = 1.d0

	  
!      時間比
!!      ttt = time_ratio_1+time_ratio_2+time_ratio_3
!!      t1 = t_burn*time_ratio_1/ttt
!!      t2 = t_burn*time_ratio_2/ttt
!!      t3 = t_burn*time_ratio_3/ttt

!      時間
!      t_burn1 = t1+t2+t3
!	  t_burn2 = t_burn1+t_not_burn
!	  t_burn3 = t_burn2 + t4
!        print *, "t_burn1", t_burn1!!!!!!!!!!
!        print *, "t_burn2", t_burn2!!!!!!!!!!
!        print *, "t_burn3", t_burn3!!!!!!!!!!
!	  if(t4 .eq. 0.d0)then
!	  t_tot = t_burn1
!	  else
 !     t_tot = t_burn3
	!  endif
	   if(t_burn.lt.t_stop_burn)then 
        t_tot=t_burn
       else
        t_tot=t_burn+t_rest_burn-t_stop_burn
       endif
!        print *, "t_tot", t_tot!!!!!!!!!!	  
!     --- Oxidizer weight calculation ---

!.kana.1312
!      if(t_rest_burn.gt.t_stop_burn)then
!       t_tot=t_burn+t_rest_burn-t_stop_burn
!      else
!       t_tot=t_burn
!      endif

!       if(t_burn.lt.t_stop_burn)then 
!        t_tot=t_burn
!       else
!        t_tot=t_burn+t_rest_burn-t_stop_burn
!       endif

     
      do n=1,nmax
        time_oxi(n) = time_oxi(n-1) + dt
!     before engine cut off
!.kana.1312

!org
        igon=0
        if(time_oxi(n).le.t_stop_burn)igon=1
        if(time_oxi(n).ge.t_rest_burn)igon=1
         
        if(time_oxi(n).le.t_tot .and.  igon.eq.1)then
!.kana.1312_en 

          m_dot_oxi(n)   = m_dot_oxi_ini
          mn_oxi(n)      = mn_oxi(n-1)+&
                          &(m_dot_oxi(n-1)+m_dot_oxi(n))*dt/2.d0
!     after engine cut off
        else
          m_dot_oxi(n)  = 0.d0
          mn_oxi(n)     = mn_oxi(n-1)
        endif
      enddo
      m_oxi(0) = mn_oxi(nmax)

!     --- Time Integration ---
      do n=1,nmax
        time(n) = time(n-1) + dt
!       before engine cut off
!.kana.1312

!org        if(time(n).le.t_burn)then
        igon=0
        if(time(n).le.t_stop_burn)igon=1
        if(time(n).ge.t_rest_burn)igon=1
        
!           print *,time(n),igon,t_burn,t_tot,t_rest_burn
!           print *,"igon,t_burn,t_tot,t_rest_burn"
           
        if(time(n).le.t_tot .and.  igon.eq.1)then
!.kana.1312_en 

            l_dot_fuel(n) = a_fuel*pc_bar**n_fuel
            l_fuel(n)     = l_fuel_ini-l_dot_fuel(n-1)*dt
            m_dot_oxi(n)  = m_dot_oxi_ini
            area_flow(n)  = pi*(r_port(n))**2.0
            go(n)         = m_dot_oxi(n)/area_flow(n)
            m_dot_fuel(n) = l_dot_fuel(n)*dens_fuel*pi*&
                           &2.0*r_port(n)*l_fuel
            of(n)         = m_dot_oxi(n)/m_dot_fuel(n)
            m_dot_prop(n) = m_dot_oxi(n)+m_dot_fuel(n)
            m_oxi(n)      = m_oxi(n-1)-&
                             &(m_dot_oxi(n-1)+m_dot_oxi(n))*dt/2.d0
            mn_fuel(n)    = mn_fuel(n-1)+&
                           &(m_dot_fuel(n-1)+m_dot_fuel(n))*dt/2.d0
!     after engine cut off
          else
            l_dot_fuel(n) = 0.d0
            r_port(n)     = r_port(n-1)
            m_dot_oxi(n)  = 0.d0
            go(n)         = 0.d0
            go_n(n)       = 0.d0
            m_dot_fuel(n) = 0.d0
            of(n)         = of(n-1)
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
      r_fuel    = (m_fuel(0)/(dens_fuel*pi*l_fuel)&
                   &+r_port_ini**2.d0)**0.5d0

!     --- Propellant weight calculation ---
      do i=0,nmax
        m_prop(i) = m_oxi(i)+m_fuel(i)
      enddo

!     --- Output ---
      open(2,file='engine-out.dat',form='formatted')
        write(2,10)'time','propellant_mass',&
          &'m_dot_oxi','m_dot_fuel','O/F',&
          &'m_oxi','m_fuel',&
          &'m_dot_prop','r_port','Go',&
          &'l_dot_fuel','flow_area'
        do i=0,nmax
          write(2,20)time(i),m_prop(i),m_dot_oxi(i),&
          &m_dot_fuel(i),of(i),m_oxi(i),m_fuel(i),&
          &m_dot_prop(i),r_port(i),go(i),&
          &l_dot_fuel(i),area_flow(i)  
        enddo
      close(2)
!------------------------------------------------------------------------------
!     --- NASA-CEA(time=0) ---
      open(15,file='nasa-cea.inp',form='formatted')
        write(15,*)'problem    o/f=',of1
        write(15,*)'    rocket  frozen  nfz=2  tcest,k=3000'
        write(15,*)'  p,bar=',pc_bar
        write(15,*)'  sup,ae/at=',epsilon
        write(15,*)'react'
        write(15,*)'  oxid=O2 wt=100  t,k=298.15'
        write(15,*)'  fuel=C3H6  wt=100  t,k=298.15  '
        write(15,*)'    h,kj/mol=-20.918  C 3 H 6 '
        write(15,*)'output'
        write(15,*)'    plot p son machfz ispfz cffz'
        write(15,*)'end'
      close(15)

      call system('echo nasa-cea > inp.txt') !echo 文字列＞ファイル名　ファイルに文字列を出力
      call system('sh cea-exe.sh') !cea-exe.sh　を実行
!	  call system('mv inp.txt in.txt')
      call system('rm inp.txt') !inp.txtを削除

!------------------------------------------------------------------------------
!     --- Structure calculation ---
!     --- Nozzle_throat_are・Nozzle_exit_area---

      open(13,file='nasa-cea.plt',form='formatted')
        read(13,*)
        read(13,*)
        read(13,*) pe_ini,son_ini,mach_ini,isp_ini,cf_ini
        read(13,*) pe, son, mach, isp0, cf
      close(13)

	  pen(0) = pe
	  sonn(0) = son
	  machn(0) = mach
	  isp(0) = isp0
	  
	  
!      cstar_ini  = isp_ini/cf_ini
!     Isp of CEA is  Isp[sec]*(gravitational acceleration)[m/sec^2]
      p_ch = pc_bar*10d04
      area_t     = m_dot_prop_ini*isp_ini*eita_isp&
                   &/(cf_ini*eita_cf*p_ch)
      area_e     = epsilon*area_t
      r_t        = (area_t/pi)**0.5
      r_e        = (area_e/pi)**0.5
      ramda      = 0.5*(1.+cos(pi*theta2/180.))

!     --- Nozzle ---
      l_noz = (r_fuel-r_t)/tan(pi*theta1/180.)&
              &+(r_e-r_t)/tan(pi*theta2/180.)
      m_noz = 125.0*((m_prop(0)/5400.0)**(2.0/3.0))&
              &*((epsilon/10.0)**0.250)

!yoda
!     --- Nozzle capacity ---
      a_noz_e = (r_e+r_t)*(r_e-r_t)*pi/(tan(pi*theta2/180.))
      a_noz_i = (r_t+r_fuel)*(r_fuel-r_t)*pi/(tan(pi*theta2/180.))
      a_noz = a_noz_e + a_noz_i

!     --- Nozzle mass ---
      mass_noz_e = m_noz*a_noz_e/a_noz
      mass_noz_i = m_noz*a_noz_i/a_noz

!     --- Nozzle CoG length ---
      cgl_noz_e = 0.25*(r_e-r_t)/tan(pi*theta2/180.)&
      &          *(3.0*(r_t/r_e)**2+2*(r_t/r_e)+1.0)&
      &          /((r_t/r_e)**2+(r_t/r_e)+1.0)
      cgl_noz_i = l_noz-0.25*(r_fuel-r_t)/tan(pi*theta1/180.)&
      &          *(3.0*(r_t/r_fuel)**2+2*(r_t/r_fuel)+1.0)&
      &          /((r_t/r_fuel)**2+(r_t/r_fuel)+1.0)

!     --- Nozzle CoG ---
      cge = cgl_noz_e*mass_noz_e
      cgi = cgl_noz_i*mass_noz_i

!     --- Nozzle projected area ---
      pa_noz = (((r_fuel**2)-(r_t**2))/(2.0*tan(pi*theta1/180.)))&
      &       +(((r_e**2)-(r_t**2))/(2.0*tan(pi*theta2/180.)))
!yoda.e


!     --- Thickness@chamber・Chamber_volume・Chamber_weight---
      l_ch = l_fuel
      t_ch = sf*(p_ch*r_fuel/sty_ch)
      v_ch = pi*((r_fuel+t_ch)**2.-r_fuel**2.)*l_ch
      m_ch = v_ch*dens_ch

!     --- Thickness@reservoir・Reservoir_volume・Reservoir_weight ---
      p_res     = 1.5*p_ch
	  if(m_oxi(0).le.dens_oxi*pi*4.d0/3.d0*r_fuel**3.)then
	  l_res_cyl = 0.d0
	  else
      l_res_cyl = m_oxi(0)/(dens_oxi*pi*r_fuel**2.)-4.*r_fuel/3.
	  endif
      t_res     = sf*(p_res*r_fuel/sty_res)
      v_res     = pi*((r_fuel+t_res)**2.-r_fuel**2.)*l_res_cyl+&
                 &4.*pi*((r_fuel+t_res)**3.-r_fuel**3.)/3.
      m_res     = v_res*dens_res
      l_res     = l_res_cyl+2.0*(r_fuel+t_res)

!     --- Engine_weight ---
      l_tot_en = l_res+l_ch+l_noz

!     --- Rocket_total_radius・Rocket_reference_area ---
      r_tot = r_fuel+t_res
      ref_area = pi*r_tot**2.

!     --- Rocket_lenght・Wet_area・Rocket_total_weight ---
      m_tot(0)     = (m_prop(0)+m_ch+m_res)/m_ratio+m_pay
      m_str        = m_tot(0)-m_prop(0)-m_pay
      do i=1,nmax
        m_tot(i)   = m_str+m_prop(i)+m_pay
      enddo
      l_tot        = l_tot_en*l_ratio
      ref_area_wet = 2.*pi*r_tot*l_tot
      ld           = l_tot/(2.0*r_tot)

!yoda
!     --- Nose corn ratio ---
      nc_ratio = 0.5

!     --- Nose corn length ---
      l_nose = (l_tot-l_tot_en)*nc_ratio
!
!     --- Nose corn projected area ---
      pa_nose = l_nose*r_fuel

!     --- Payload length ---
      l_pay = (l_tot-l_tot_en)*(1.0-nc_ratio)

!     --- m_wall ---
      m_wall = m_str-m_ch-m_res

!     --- Unit CoG ---
      do i=0,nmax
      cg_ch(i) = (l_noz+(l_ch/2.0))&
      &        *(m_ch+m_fuel(i)+(m_wall*l_ch/(l_tot-l_noz-0.5*l_nose)))
      cg_res(i) = (l_noz+l_ch+(l_res/2.0))&
      &        *(m_res+m_oxi(i)+(m_wall*l_res/(l_tot-l_noz-0.5*l_nose)))
      enddo
      cg_pay = (l_tot_en+l_pay/2.0)&
      &         *(m_pay+(m_wall*l_pay/(l_tot-l_noz-0.5*l_nose)))
      cg_nose = (l_tot_en+l_pay+l_nose/4.0)&
      &         *(m_wall*l_nose/(l_tot-l_noz-0.5*l_nose))
!yoda.e
!-------------------------------------------------------------------------------
!yoda
! Center of Gravity  Center of Pressure

!     --- CoG (t=0) (from bottom of rocket)
      cg(0)=(cge+cgi+cg_ch(0)+cg_res(0)+cg_pay+cg_nose)/m_tot(0)

!     --- CoG(t>0) (from bottom of rocket) ---
      do n=1,nmax
      cg(n)=(cge+cgi+cg_ch(n)+cg_res(n)+cg_pay+cg_nose)/m_tot(n)
      enddo

!     --- Center of pressure (from bottom of rocket) ---
      cp = (pa_nose-pa_noz+2.0*l_tot*r_fuel+2.0*l_noz*r_fuel-2.0&
      &   *l_nose*r_fuel)/(4.0*r_fuel)
!yoda.e
!-------------------------------------------------------------------------------
!yoda
! Moment of inertia
!     --- Moment of Inertia(t=0) ---
       mi(0) = (mass_noz_e*(abs(cg(0)-cgl_noz_e))**2)&
      &       +(mass_noz_i*(abs(cg(0)-cgl_noz_i))**2)&
      &       +(m_ch+m_fuel(0)+(m_wall*l_ch/(l_tot-l_noz)))&
      &       *(abs(cg(0)-(l_noz+(l_ch/2.0))))**2&
      &       +(m_res+m_oxi(0)+(m_wall*l_res/(l_tot-l_noz)))&
      &       *(abs(cg(0)-(l_noz+l_ch+(l_res/2.0))))*2&
      &       +(m_pay+(m_wall*(l_pay+l_nose)/(l_tot-l_noz))&
      &       *(3.0*(1.0-nc_ratio)/(3.0-(2.0*nc_ratio))))&
      &       *(abs(cg(0)-(l_tot_en+(l_pay/2.0))))**2&
      &       +(m_wall*(l_pay+l_nose)/(l_tot-l_noz))&
      &       *(nc_ratio/(3.0-(2.0*nc_ratio)))&
      &       *(abs(cg(0)-(l_tot_en+l_pay+(l_nose/3.0))))**2

!     --- Moment of Inertia(t>0) ---
      do n=1,nmax
      mi(n) = (mass_noz_e*(abs(cg(n)-cgl_noz_e))**2)&
      &      +(mass_noz_i*(abs(cg(n)-cgl_noz_i))**2)&
      &      +(m_ch+m_fuel(n)+(m_wall*l_ch/(l_tot-l_noz)))&
      &      *(abs(cg(n)-(l_noz+(l_ch/2.0))))**2&
      &      +(m_res+m_oxi(n)+(m_wall*l_res/(l_tot-l_noz)))&
      &      *(abs(cg(n)-(l_noz+l_ch+(l_res/2.0))))*2&
      &      +(m_pay+(m_wall*(l_pay+l_nose)/(l_tot-l_noz))&
      &      *(3.0*(1.0-nc_ratio)/(3.0-(2.0*nc_ratio))))&
      &      *(abs(cg(n)-(l_tot_en+(l_pay/2.0))))**2&
      &      +(m_wall*(l_pay+l_nose)/(l_tot-l_noz))&
      &      *(nc_ratio/(3.0-(2.0*nc_ratio)))&
      &      *(abs(cg(n)-(l_tot_en+l_pay+(l_nose/3.0))))**2
      enddo
!yoda.e

!-------------------------------------------------------------------------------

!     --- Output Structure ---
      open(4,file='engine-str-out.dat',form='formatted')
        write(4,*) p_res            ,'Pressure@reservoir[Pa]'
        write(4,*) p_ch             ,'Pressure@chamber[Pa]'
        write(4,*) m_oxi(0)         ,'Oxidizer weight[kg]'
        write(4,*) m_fuel(0)        ,'Fuel weight[kg]'
        write(4,*) r_fuel           ,'Radius@chamber inside[m]'
        write(4,*) area_t           ,'Area@throat[m^2]'
        write(4,*) area_e           ,'Area@exit[m^2]'
        write(4,*) r_t              ,'Radius@throat[m^2]'
        write(4,*) r_e              ,'Radius@exit[m^2]'
        write(4,*) l_noz            ,'Nozzle length[m]'
        write(4,*) m_noz            ,'Nozzle weight[m]'
        write(4,*) l_ch             ,'Chamber length[m]'
        write(4,*) t_ch             ,'Chamber thicknes[m]'
        write(4,*) m_ch             ,'Chamber weight[kg]'
        write(4,*) l_res            ,'Reservoir length[m]'
        write(4,*) l_res_cyl        ,'Reservoir cylinder length[m]'
        write(4,*) t_res            ,'Reservoir thickness[m]'
        write(4,*) m_res            ,'Reservoir weight[kg]'
        write(4,*) r_tot            ,'Rocket radius[m]'
        write(4,*) l_tot_en         ,'Total engine length[m]'
        write(4,*) l_tot            ,'Total rocket length[m]'
        write(4,*) m_str            ,'Rocket structure weight[kg]'
        write(4,*) m_pay            ,'Payload weight[kg]'
        write(4,*) m_tot(0)         ,'Total rocket weight[kg]'
        write(4,*) ref_area         ,'Rocket refence area[m^2]'
        write(4,*) ref_area_wet     ,'Total rocket wet area[m^2]'
        write(4,*) ld               ,'L/D[-]'
      close(4)

!------------------------------------------------------------------------------
!     --- Trajectory calculation(time=0) ---
!     --- Input data ---
      t_atm1 = 288.16                  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      p_atm1 = 101325.0                !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      sound1 = 340.4                   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      nyu1   = 1.46*10**(-5.)          !!!!!!!!!!!!!!!!!!!!!!!!!!!!!

      cross1 = 0.0
      hei1   = 0.0
      vel1   = 0.0
      vel1c  = 0.0
      acc1   = 0.0
      acc1c  = 0.0
      theta_l1 = theta_ini
      acc_theta_p1 = 0.0
      omega1 = 0.0
      theta_p1 = theta_ini

!kana      mach1  = abs(vel1/sound1)
      mach1  = abs(sqrt(vel1*vel1+vel1c*vel1c)/sound1)
!     --- Frictional drag ---
      cft_s520 = 0.0         ! nuremensekikijyun
      cft_s520_ref = 0.0     ! danmensekikijyun
      cft_des(0) = 0.0 
	  cft_s520i(0) = cft_s520
    
!     --- Pressure drag ---
      cd_s520 = 0.43
      cdp_s520 = cd_s520-cft_s520_ref
      cdp_des(0)  = cdp_s520

!     --- Translating the cd into the designed rocket cd ---
      drag0  = 0.5*dens_atm0*(sqrt(vel1**2+vel1c**2))**2.*&
              &(ref_area*cdp_des(0)+ref_area_wet*cft_des(0))
      drag(0)   = drag0*exp(-alpha*hei1)
      dens_atml(0) = dens_atm0*exp(-alpha*hei1)

!     --- Thrust patterns ---
      ue     = son*mach
      thrust(0) = eita_T*(ramda*m_dot_prop(0)*ue+(pe*10**5.-p_atm1)*area_e)
	  
	  
	  thrust1n(0) = eita_T*(ramda*m_dot_prop(0)*ue)
	  thrust2n(0) = eita_T*((pe*10**5.-p_atm1)*area_e)
	  uen(0) = ue

!     --- Trajectory analysis ---

      if(vel1.gt.0.)then
        accn2  = sin(theta_p1*pi/180.)*(thrust(0)-drag(0))/m_tot(0)-gravity
!kana        accn2  = (thrust(0)-drag(0))/m_tot(0)-gravity
      elseif(vel1.lt.0.0)then
        accn2  = sin(theta_p1*pi/180.)*(thrust(0)+drag(0))/m_tot(0)-gravity
!kana         accn2  = (thrust(0)+drag(0))/m_tot(0)-gravity
      else
        accn2  = sin(theta_p1*pi/180.)*thrust(0)/m_tot(0)-gravity
!kana         accn2  = thrust(0)/m_tot(0)-gravity
      endif

      if(vel1c.gt.0.)then
        accn2c = cos(theta_p1*pi/180.)*(thrust(0)-drag(0))/m_tot(0)
      elseif(vel1c.lt.0.0)then
        accn2c = cos(theta_p1*pi/180.)*(thrust(0)+drag(0))/m_tot(0)
      else
        accn2c = cos(theta_p1*pi/180.)*thrust(0)/m_tot(0)
      endif

!kana
      veln2c = vel1c +(accn2c+acc1c)*dt/2.0
      cron2  = cross1+(veln2c+vel1c)*dt/2.0
!kana.e
      veln2  = vel1 +(accn2+acc1)*dt/2.0
      hein2  = hei1  +(veln2+vel1)*dt/2.0

!yoda
!     --- Theta gap ---
!      gap_theta = theta_p1-atan(veln2/veln2c)
!      if(gap_theta.le.90.)then
!          gap_theta0 = theta_p1-(atan(veln2/veln2c)*180.0/pi)
!      elseif(gap_theta.gt.90.)then
!          gap_theta0 = 90.0-(theta_p1-(atan(veln2/veln2c)*180.0/pi))
!          endif

      gap_theta0 = 0.d0
		  
      gap(0) = gap_theta0

!     --- Lift calculation ---
      lift0 = dens_atml(0)*((vel1c**2.0)+(vel1**2.0))*(pi*r_tot**2.0)&
      &      *gap_theta0*pi/180.0

!     --- Thrust tendency ---
      thrust_x(0) = 0
      thrust_y(0) = 0

!     --- Acceleration of theta_p ---
	   if(hein2 .lt. l_tot*1.2*sin(theta_ini*pi/180.0))then
	      acc_theta_pn2 = 0.
		  omegan2 = 0.
		  theta_pn2 = theta_p1
		  theta_ln2 = theta_l1
	   else
          acc_theta_pn2 = lift0*(((l_tot-cg(0))-(l_tot-cp))+thrust_y(0)&
      &             *(l_tot-(l_tot-cg(0))))/mi(0)
          omegan2 = omega1 + (acc_theta_pn2 + acc_theta_p1) * dt/2.0	  
	      theta_pn2 = theta_p1 + (omegan2 + omega1) * dt/2.0
		  theta_ln2 = atan(veln2/veln2c)*180.0/pi
	   endif

      if(hein2.lt.0.) then
        cro2(0) = 0.
        hei2(0) = 0.
        vel2c(0)= 0.
        vel2(0) = 0.
        acc2c(0)= 0.
        acc2(0) = 0.
        theta_l2(0) = 0.
	    acc_theta_p2(0) = 0.
		omega2(0) = 0.
	    theta_p2(0) = 0.
	    lift(0) = 0.
	    rey(0) = 0.
      else
        cro2(0) = cron2
        hei2(0) = hein2
        vel2c(0)= veln2c
        vel2(0) = veln2
        acc2c(0) = accn2c
        acc2(0) = accn2
        theta_l2(0) = theta_ln2
		acc_theta_p2(0) = acc_theta_pn2
        omega2(0) = omegan2
		theta_p2(0) = theta_pn2
	    lift(0) = lift0
	    rey(0) = reynolds
      endif
	  
!      print *,'check',cro2(0),hei2(0),vel2(0),acc2c(0),acc2(0),theta_l(0)

!     --- Atmosphere calculation ---
      t_atm2(0) = -6.5*10**(-3.)*hei2(0)+288.16
      p_atm2(0) = 101325.*(t_atm2(0)/288.16)**&
               &((-9.80665)/(-6.5*10**(-3.)*287.04))
      sound2(0) = (1.4*287.*t_atm2(0))**0.5
!kana      mach2  = abs(vel2(0)/sound2(0))
      mach2  = abs(sqrt(vel2(0)**2.0+vel2c(0)**2.0)/sound2(0))
      nyu2(0) = 1.46*10**(-5.)*exp(0.0877*hei2(0)/1000.)
	  ro2(0) = 1.48*exp(-0.000133*hei2(0))
	  
!	  call system('mv nasa-cea.plt nasacea.plt')

      call system('rm nasa-cea.plt')

!------------------------------------------------------------------------------
!     --- Trajectory calculation(time>0) ---
      do n=1,nmax
      t_atm1 = t_atm2(n-1)
      p_atm1 = p_atm2(n-1)
      sound1 = sound2(n-1)
      nyu1   = nyu2(n-1)
	  ro1 = ro2(n-1)

      cross1 = cro2(n-1)
      hei1   = hei2(n-1)
      vel1   = vel2(n-1)
      vel1c  = vel2c(n-1)
      acc1   = acc2(n-1)
      acc1c  = acc2c(n-1)
      theta_l1 = theta_l2(n-1)
	  acc_theta_p1 = acc_theta_p2(n-1)
      omega1 = omega2(n-1)
      theta_p1 = theta_p2(n-1)

!      mach1  = abs(vel1/sound1)
      mach1  = abs(sqrt(vel1*vel1+vel1c*vel1c)/sound1)


!yoda
!     --- Theta gap ---
      gap_theta = theta_p1-theta_l1
!      if(gap_theta.ge.-90.0 .and. gap_theta.le.90.0)then
!          gap_theta0 = theta_p1-theta_l1
!      elseif(gap_theta0.ge.90.0 .and. gap_theta.lt.180.)then
!          gap_theta0 = (theta_p1-theta_l1)-90.0
!      else
!          gap_theta0 = (theta_p1-theta_l1)-180.0
!      endif

      if(gap_theta/360.0 .ge. 1.0 .or. gap_theta/360.0 .le. -1.0)then
	     gap_theta = mod(gap_theta , 360.0)
	  endif

      if(gap_theta.ge.85.0 .and. gap_theta.le.95.0)then
	      gap_theta0 = 85.0
       elseif(gap_theta.ge.175.0 .and. gap_theta.le.185.0)then
	      gap_theta0 = 175.0
	   elseif(gap_theta.ge.265.0 .and. gap_theta.le.275.0)then
	      gap_theta0 = 265.0
	   elseif(gap_theta.ge.-95.0 .and. gap_theta.le.-85.0)then
	      gap_theta0 = -85.0
	   elseif(gap_theta.ge.-185.0 .and. gap_theta.le.-175.0)then
	      gap_theta0 = -175.0
	   elseif(gap_theta.ge.-275.0 .and. gap_theta.le.-265.0)then
	      gap_theta0 = -265.0
	   else
	      gap_theta0 = gap_theta
	  endif
	  cosgap = abs(cos(gap_theta0*pi/180.0))
	  singap = abs(sin(gap_theta0*pi/180.0))
	  gap(n) = gap_theta0
	  


!     1,nennsyoutyuu
!org
!       if(time(n).lt.t_burn)then
!.kana.1312

        igon=0
        if(time(n).le.t_burn1)igon=1
        if(time(n).ge.t_burn2)igon=1
        
!           print *,"igon,t_burn,t_tot,t_rest_burn"
           
        if(time(n).lt.t_tot .and.  igon.eq.1)then
!.kana.1312_en 
!		 print *, 't1',time(n)
         if( (time(n) .eq. t1).and.(of2.ne.0.0) .or. (time(n) .eq. t1+t2).and.(of3.ne.0.0) .or. (time(n) .eq. t_burn2))then
!		 print *, 'aaaaa',time(n)

!     --- NASA-CEA ---
        open(15,file='nasa-cea.inp',form='formatted')
          write(15,*)'problem    o/f=',of(n)
          write(15,*)'    rocket  frozen  nfz=2  tcest,k=3000'
          write(15,*)'  p,bar=',pc_bar
          write(15,*)'  sup,ae/at=',epsilon
          write(15,*)'react'
          write(15,*)'  oxid=O2 wt=100  t,k=298.15'
          write(15,*)'  fuel=C3H6  wt=100  t,k=298.15  '
          write(15,*)'    h,kj/mol=-20.918  C 3 H 6 '
          write(15,*)'output'
          write(15,*)'    plot p son machfz ispfz cffz'
          write(15,*)'end'
        close(15)
     
        call system('echo nasa-cea > inp.txt')
        call system('sh cea-exe.sh')
        call system('rm inp.txt')

!     --- Input data ---
        open(16,file='nasa-cea.plt',form='formatted')
           read(16,*)
           read(16,*)
           read(16,*) pe_ini,son_ini,mach_ini,isp_th(n),cf_th(n)
           read(16,*) pe, son, mach, isp0, cf
         close(16)
!		 print *,pe, son, mach, isp0
	     endif
		 

		 
		 pen(n) = pe
		 sonn(n) = son
		 machn(n) = mach
!		 if(time(n).lt.t1)then
		 isp(n) = isp0
!		 else
!		 isp(n) = isp0
!		 endif

!     --- Frictional drag for S-520 ---
         if(mach1.le.0.01)then
           cft_s520 = 0.         ! nuremennsekikijyun
           cft_s520_ref = 0.     ! dannmennsekikijyun
         else
           reynolds = abs(sqrt(vel1**2+vel1c**2))&
      &              *cosgap*8.715/nyu1
           cft_s520     = (0.455/(alog10(reynolds))**2.58)/&
                         &(1.+0.144*(mach1*cosgap)**2.)**0.65
           cft_s520_ref = cft_s520*14.3466/0.215651
         endif

!yoda
!     --- Pressure drag ---
         if(mach1.le.0.25)then
           cd_s520 = 0.43
         elseif(mach1.gt.0.25.and.mach1.le.0.625)then
           cd_s520 = -0.08*(mach1*cosgap)+0.45
         elseif(mach1.gt.0.625.and.mach1.le.1.25)then
           cd_s520 = 0.428*(mach1*cosgap)**2.&
      &             -0.292*(mach1*cosgap)+0.42
         elseif(mach1.gt.0.125.and.mach1.le.4.)then
           cd_s520 = 0.763*(mach1*cosgap)**(-0.63)
         elseif(mach1.gt.4..and.mach1.le.6.)then
           cd_s520 = 0.577*(mach1*cosgap)**(-0.41)
         else
           cd_s520 = 0.275
         endif

         cdp_s520 = cd_s520-cft_s520_ref
         cdp_des(n)  = cdp_s520

!     --- Frictional drag for Design rocket ---
         if(mach1.le.0.01)then
           cft_des(n) = 0.  
         else
           reynolds = abs(sqrt(vel1*vel1+vel1c*vel1c))&
      &              *cosgap*l_tot/nyu1
           cft_des(n)   = (0.455/(alog10(reynolds))**2.58)/&
                         &(1.+0.144*(mach1*cosgap)**2.)&
						 &**0.65
         endif

!     --- Translating the cd into the designed rocket cd ---
         drag0 = 0.5*dens_atm0*(sqrt(vel1*vel1+vel1c*vel1c)&
      &         *cosgap)**2.&
      &         *(ref_area*cdp_des(n)+ref_area_wet*cft_des(n))
         drag(n) = drag0*exp(-alpha*hei1)/cosgap
         dens_atml(n) = dens_atm0*exp(-alpha*hei1)

!     --- Thrust patterns ---
         ue     = son*mach
         thrust(n) = eita_T*(ramda*m_dot_prop(n)*ue+(pe*10**5.-p_atm1)*area_e)
		 
	     thrust1n(n) = eita_T*(ramda*m_dot_prop(n)*ue)
	     thrust2n(n) = eita_T*((pe*10**5.-p_atm1)*area_e)
		 uen(n) = ue

		 
         if( (time(n) .eq. t1).and.(of2.ne.0.0) .or. (time(n) .eq. t1+t2).and.(of3.ne.0.0) .or. (time(n) .eq. t_burn2))then
         call system('rm nasa-cea.plt')
		 endif

!     2,燃焼終了後
      else
!     --- Frictional drag for S-520 ---
         if(mach1.le.0.01)then
           cft_s520 = 0.         ! nuremennsekikijyun
           cft_s520_ref = 0.     ! dannmennsekikijyun
         else
           reynolds = abs(sqrt(vel1*vel1+vel1c*vel1c))&
      &              *cosgap*8.715/nyu1
           cft_s520 = (0.455/(alog10(reynolds))**2.58)&
      &             /(1.+0.144*(mach1*cosgap)**2.)**0.65
           cft_s520_ref = cft_s520*14.3466/0.215651  
         endif

!     --- Pressure drag ---
         if(mach1.le.0.25)then
           cd_s520 = 0.49
         elseif(mach1.gt.0.25.and.mach1.le.0.625)then
           cd_s520 = -0.04*(mach1*cosgap)+0.5
         elseif(mach1.gt.0.625.and.mach1.le.1.25)then
           cd_s520 = 0.8563*(mach1*cosgap)**2.&
      &             -0.9854*(mach1*cosgap)+0.7655
         elseif(mach1.gt.1.25.and.mach1.le.7.)then
           cd_s520 = 0.9168*(mach1*cosgap)**(-0.635)
         else
           cd_s520 = 0.275
         endif

         cdp_s520 = cd_s520-cft_s520_ref
         cdp_des(n)  = cdp_s520

!     --- Frictional drag for Design rocket ---
         if(mach1.le.0.01)then
           cft_des(n) = 0. 
         else
           reynolds = abs(sqrt(vel1*vel1+vel1c*vel1c))&
      &              *cosgap*l_tot/nyu1
           cft_des(n)   = (0.455/(alog10(reynolds))**2.58)/&
               &(1.+0.144*(mach1*cosgap)**2.)**0.65
         endif

!     --- Translating the cd into the designed rocket cd ---
         drag0  = 0.5*dens_atm0*(sqrt(vel1*vel1+vel1c*vel1c)&
      &          *cosgap)**2.&
      &          *(ref_area*cdp_des(n)+ref_area_wet*cft_des(n))
         drag(n)   = drag0*exp(-alpha*hei1)/cosgap
         dens_atml(n) = dens_atm0*exp(-alpha*hei1)

!     --- Thrust patterns ---
         thrust(n) = 0.0
      endif

      cft_s520i(n) = mach1

!     --- Trajectory analysis ---

      if(vel1.gt.0.)then
      accn2  = ((sin(theta_p1*pi/180.)*thrust(n))-&
	  &      (drag(n)*sin(theta_l1*pi/180)))/m_tot(n)-gravity
!yoda        accn2 = sin(theta_l0*pi/180.)*(thrust(n)-drag(n))/m_tot(n)-gravity
!kana        accn2  = (thrust(n)-drag(n))/m_tot(n)-gravity
      elseif(vel1.lt.0.0)then
      accn2  = ((sin(theta_p1*pi/180.)*thrust(n))-&
	  &       (drag(n)*sin(theta_l1*pi/180)))/m_tot(n)-gravity
!yoda        accn2  = sin(theta_l0*pi/180.)*(thrust(n)+drag(n))/m_tot(n)-gravity
!kana        accn2  = (thrust(n)+drag(n))/m_tot(n)-gravity
      else
        accn2  = sin(theta_p1*pi/180.)*thrust(n)/m_tot(n)-gravity
!kana         accn2  = thrust(n)/m_tot(n)-gravity
      endif

      if(vel1c.gt.0.)then
        accn2c = ((abs(cos(theta_p1*pi/180.))*thrust(n))&
		&        -(drag(n)*abs(cos(theta_l1*pi/180))))/m_tot(n)
!yoda        accn2c = cos(theta_l0*pi/180.)*(thrust(n)-drag(n))/m_tot(n)
      elseif(vel1c.lt.0.0)then
       accn2c = ((abs(cos(theta_p1*pi/180.))*thrust(n))&
	   &      +(drag(n)*abs(cos(theta_l1*pi/180))))/m_tot(n)
!yoda        accn2c = cos(theta_l0*pi/180.)*(thrust(n)+drag(n))/m_tot(n)
      else
        accn2c = abs(cos(theta_p1*pi/180.))*thrust(n)/m_tot(n)
      endif

!kana
      veln2c = vel1c +(accn2c+acc1c)*dt/2.0
      cron2  = cross1+(veln2c+vel1c)*dt/2.0
!kana.e
      veln2  = vel1+(accn2+acc1)*dt/2.0
      hein2  = hei1+(veln2+vel1)*dt/2.0

!yoda
!     --- Lift calculation ---

      if(gap_theta0.ge.90.0 .and. gap_theta0.lt.180.0)then
	     gap_theta0 = 180.0 - gap_theta0
	  elseif(gap_theta0.ge.180.0 .and. gap_theta0.lt.270.0)then
	     gap_theta0 = gap_theta0 - 180.0
	  elseif(gap_theta0.ge.270.0 .and. gap_theta0.lt.360.0)then
	     gap_theta0 = 360.0 - gap_theta0
	  elseif(gap_theta0.ge.-180.0 .and. gap_theta0.lt.-90.0)then
	     gap_theta0 = -180.0 - gap_theta0
	  elseif(gap_theta0.ge.-270.0 .and. gap_theta0.lt.-180.0)then
	     gap_theta0 = gap_theta0 + 180.0
	  elseif(gap_theta0.ge.-360.0 .and. gap_theta0.lt.-270.0)then
	     gap_theta0 = -360.0 - gap_theta0
	  else
	      gap_theta0 = gap_theta0
	  endif
      lift0 = dens_atml(n)*((veln2c**2.0)+(veln2**2.0))&
	  &      *(pi*r_tot**2.0)*gap_theta0*pi/180.0

!     --- Thrust tendency ---
      thrust_x(n) = 0
      thrust_y(n) = 0

!     --- Accel of theta_p ---
!      print *,hein2
	   if(hein2 .lt. l_tot*1.2*sin(theta_ini*pi/180.0))then
	      acc_theta_pn2 = 0.
		  omegan2 = 0.
		  theta_pn2 = theta_p1
		  theta_ln2 = theta_l1
	   else
	      if(gap_theta0.ge.0.0 .and. gap_theta0.lt.180.0)then
          acc_theta_pn2 = (abs(lift0)*cosgap+drag(n)*singap)*(((l_tot-cg(n))-(l_tot-cp))+thrust_y(n)&
      &             *(l_tot-(l_tot-cg(n))))/mi(n)
	      elseif(gap_theta0.gt.-360.0 .and. gap_theta0.le.-180.0)then
          acc_theta_pn2 = (abs(lift0)*cosgap+drag(n)*singap)*(((l_tot-cg(n))-(l_tot-cp))+thrust_y(n)&
      &             *(l_tot-(l_tot-cg(n))))/mi(n)
          else		  
          acc_theta_pn2 = -(abs(lift0)*cosgap+drag(n)*singap)*(((l_tot-cg(n))-(l_tot-cp))+thrust_y(n)&
      &             *(l_tot-(l_tot-cg(n))))/mi(n)
	      endif

          omegan2 = omega1 + (acc_theta_pn2 + acc_theta_p1) * dt/2.0	  
	      theta_pn2 = theta_p1 + (omegan2 + omega1) * dt/2.0
		  theta_ln2 = atan(veln2/veln2c)*180.0/pi
	   endif

!yoda.e

      if(hein2.lt.0.) then
        cro2(n) = cro2(n-1)
        hei2(n) = 0.
        vel2c(n)= 0.
        vel2(n) = 0.
        acc2c(n)= 0.
        acc2(n) = 0.
        theta_l2(n) = 0.
        acc_theta_p2(n) = 0.
        omega2(n) = 0.
        theta_p2(n) = 0.
        lift(n) = 0.
		rey(n) = 0.

      else
        cro2(n) = cron2
        hei2(n) = hein2
        vel2c(n)= veln2c
        vel2(n) = veln2
        acc2c(n) = accn2c
        acc2(n) = accn2
        theta_l2(n) = theta_ln2
        acc_theta_p2(n) = acc_theta_pn2
		omega2(n) = omegan2
        theta_p2(n) = theta_pn2
        lift(n) = lift0
		rey(n) = reynolds
      endif

!     --- Atmosphere calculation ---
      if(hei2(n).lt.11000.)then
        t_atm2(n) = -6.5*10**(-3.)*hei2(n)+288.16
        p_atm2(n) = 101325.*(t_atm2(n)/288.16)**&
                 &((-9.80665)/(-6.5*10**(-3.)*287.04))
      elseif(hei2(n).ge.11000..and.hei2(n).lt.25000.)then
        t_atm2(n) = 216.66
        p_atm2(n) = 22630.6991*exp(-9.80665*(hei2(n)-11000.)&
                 &/(287.04*216.66))
      elseif(hei2(n).ge.25000..and.hei2(n).lt.47000.)then
        t_atm2(n) = 0.003*hei2(n)+141.66
        p_atm2(n) = 2488.28573*(t_atm2(n)/216.66)**&
                 &((-9.80665)/(3.*10**(-3.)*287.04))
      elseif(hei2(n).ge.47000..and.hei2(n).lt.53000.)then
        t_atm2(n) = 282.66
        p_atm2(n) = 120.413541*exp(-9.80665*(hei2(n)-47000.)&
                 &/(287.04*282.66))
      elseif(hei2(n).ge.53000..and.hei2(n).lt.79000.)then
        t_atm2(n) = -0.0045*hei2(n)+521.16
        p_atm2(n) = 58.3053854*(t_atm2(n)/282.66)**&
                 &((-9.80665)/(-4.5*10**(-3.)*287.04))
      elseif(hei2(n).ge.79000..and.hei2(n).lt.90000.)then
        t_atm2(n) = 165.66
        p_atm2(n) = 1.00904382*exp(-9.80665*(hei2(n)-79000.)&
                 &/(287.04*165.66))
      elseif(hei2(n).ge.90000..and.hei2(n).lt.105000.)then
        t_atm2(n) = 0.004*hei2(n)-194.34
        p_atm2(n) = 0.104386846*(t_atm2(n)/165.66)**&
                 &((-9.80665)/(4.*10**(-3.)*287.04))
      else
        t_atm2(n) = 210.0
        p_atm2(n) = 0.007448401
      endif
	  ro2(n) = 1.48*exp(-0.000133*hei2(n))

      sound2(n) = (1.4*287.*t_atm2(n))**0.5
!.kana      mach2  = abs(vel2(n)/sound2(n))
      mach2  = abs(sqrt(vel2(n)**2.+vel2c(n)**2.)/sound2(n))

      if(hei2(n).lt.10000.)then
        nyu2(n) = 1.46*10**(-5.)*exp(0.0877*hei2(n)/1000.)
      elseif(hei2(n).ge.10000..and.hei2(n).lt.40000.)then
        nyu2(n) = 7.*10**(-6.)*exp(0.1558*hei2(n)/1000.)
      elseif(hei2(n).ge.40000..and.hei2(n).lt.100000.)then
        nyu2(n) = 3.*10**(-5.)*exp(0.1225*hei2(n)/1000.)
      else
        nyu2(n) = 3.*10**(-5.)*exp(0.1225*100.)
      endif

      enddo


!------------------------------------------------------------------------------
!     Fitness function value
!.kana  
      time1=0.0
      time2=0.0
		
      duration_time1 = 0.0
	  duration_time2 = 0.0
	  duration_time3 = 0.0
      duration_time4 = 0.0
	  
	  cross_range1 = 0.0
	  cross_range2 = 0.0
	  cross_range3 = 0.0
	  cross_range4 = 0.0
	  
	  d_pres(0) = 0.0

!.kana.e
      do i=1,nmax
        time(i) = time(i-1) + dt
        altmax  = max(altmax,hei2(i))
        cromax = max(cromax,cro2(i))
		vel=sqrt(vel2(i)**2.0+vel2c(i)**2.0)		
		d_pres(i)=0.5*ro2(i)*vel**2.0	
		if(hei2(i).eq.altmax)then
        acc=sqrt(acc2(i)**2.0+acc2c(i)**2.0)


		d_presmax=max(d_presmax, d_pres(i))
        if(acc2(i).ge.0.0 .and. acc2c(i).ge.0.0)then
         accmax  = max(accmax,acc)
        else
         accmin  = max(accmin,acc)
        endif
		endif

!.kana        print *,'check',acc2(i),acc2c(i)
!.kana        accmax  = max(accmax,sqrt(acc2(i)**2.0+acc2c(i)**2.0))

!       if((hei2(i-1).lt.100000.).and.(hei2(i).ge.100000.))then
        if((hei2(i-1).lt.50000.).and.(hei2(i).ge.50000.))then
!        if((hei2(i-1).lt.80000.).and.(hei2(i).ge.80000.))then
          time1 = time(i)
        endif
!        if((hei2(i-1).ge.100000.).and.(hei2(i).lt.100000.))then
        if((hei2(i-1).ge.50000.).and.(hei2(i).lt.50000.))then
!        if((hei2(i-1).ge.80000.).and.(hei2(i).lt.80000.))then
          time2 = time(i)
        endif

! chi      enddo
! chi: for scientific observation of low thermosphere (90km to 150km)
! 1.   crossrange
! 2.   duration time
        
        if ((hei2(i-1).LT.50000.) .and. (hei2(i).GE.50000.)) then
           cross_range1  =cro2(i)
           duration_time1=time(i)
        endif
        if ((hei2(i-1).GE.50000.) .and. (hei2(i).LT.50000.)) then
           cross_range2  =cro2(i)
           duration_time2=time(i)
        endif

        if ((hei2(i-1).LT.500000.) .and. (hei2(i).GE.500000.)) then
           cross_range3  =cro2(i)
           duration_time3=time(i)
        endif
        if ((hei2(i-1).GE.500000.) .and. (hei2(i).LT.500000.)) then
           cross_range4  =cro2(i)
           duration_time4=time(i)
        endif
      enddo

      cross_range_LT  =  cross_range2-cross_range1&
                      &-(cross_range4-cross_range3)
      cross_range_LT  = cross_range_LT*0.001
      duration_time_LT=  duration_time2-duration_time1&
                      &-(duration_time4-duration_time3)
!chi.en

      altmax  = altmax/1000.0
      cromax  = cromax/1000.0
      time100 = time2 - time1
	  tw_ratio = thrust(0)/(m_tot(0)*9.8)

      open(7,file='fitness.dat',form='formatted')
        write(7,*) cromax           , " Maximum cross range[km] "
        write(7,*) altmax           , " Maximum altitude[km]"
        write(7,*) accmax           , " Maximum acceleration[m/sec^2]"
        write(7,*) accmin           , " Maximum deceleration[m/sec^2]"
        write(7,*) time100          , " Duration time over 100km[sec]"
        write(7,*) cross_range_LT   , " CrossRange in Low Thermosphere[km]"
        write(7,*) duration_time_LT , " DirationTime Low Thermosphere[sec]"
        write(7,*) m_tot(0)         , " Total rocket weight[kg]"
        write(7,*) m_pay            , " Payload weight[kg]"
        write(7,*) m_oxi(0)         , " Oxidizer weight[kg]"
        write(7,*) m_fuel(0)        , " Fuel weight[kg]"
        write(7,*) l_tot            , " Total rokcket lenght[m]"
        write(7,*) l_noz            , " Nozzle lenght[m]"
        write(7,*) l_ch             , " Chamber lenght[m]"
        write(7,*) l_res            , " LOX tank lenght[m]"
        write(7,*) r_tot            , " Rocket outer radiusp[m]"
        write(7,*) ld               , " L/D[-]"
        write(7,*) area_t           , " Throat area[m^2]"
        write(7,*) thrust(0)/1000.0 , " Thrust[kN]"
        write(7,*) go_ini           , " Go[kg/m2s]"
		write(7,*) l_factor          , "l_factor"
		write(7,*) tw_ratio         , "thrust weight ratio"
		write(7,*) d_presmax/1000.0     , "max dynamic pressure[kPa]"
      close(7)

      open(8,file='trajectory.dat',form='formatted')
        write(8,*)'Time ','Weight ','DownRange ','Altitude ',&
                  &'Velocity_x ','Velocity_y ',&
				  &'acc_x ','acc_y ',&
				  &'theta_l ','theta_p ','gap_theta ','omega ','acc_theta_p ',&
                  &'Thrust ','Drag ','Cdp ','Cft ','Lift ',&
                  &'P_atm ','Isp ','Nyu ','Sound_vel ',&
				  &'of ','alpha_fuel_temp ',&
				  &'ue ','pe ','son ','mach ','thrust_unndouryou ','thrust_aturyoku ',&
				  &'cg ','cp ','ro ','Temperature ','Dynamic_pressure[kPa] '
        do i=0,nmax
        write(8,*) i*dt, m_tot(i), cro2(i), hei2(i),&
            	   & vel2c(i), vel2(i),&
		           & acc2c(i), acc2(i),&
				   & theta_l2(i), theta_p2(i), gap(i), omega2(i), acc_theta_p2(i),&
                   & thrust(i), drag(i), cdp_des(i), cft_des(i), lift(i),&
                   & p_atm2(i), isp(i)/gravity, nyu2(i), sound2(i), &
				   &  of(i), alpha_fuel_temp(i),&
				   & uen(i), pen(i), sonn(i), machn(i), thrust1n(i), thrust2n(i),&
				   & cg(i), cp, ro2(i), t_atm2(i), d_pres(i)/1000.0
        enddo
      close(8)

!	  open(9,file='kakunin.dat',form='formatted')
!	     write(9,*) ramda       ,"ramda"
!		 write(9,*) area_e      ,"ara_e"
!	 close(9)
	  
      call system('rm nasa-cea.inp')
      call system('rm nasa-cea.out')

      end

